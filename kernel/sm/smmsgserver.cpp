#include "smmsgserver.h"

#include "common/lockmantypes.h"
#include "common/llcommon/llMain.h"
#include "common/structures/config_data.h"

#include "sm/lm/lm_globals.h"
#include "sm/lockwarden.h"
#include "sm/trmgr.h"
#include "sm/wu/wu.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/sm_globals.h"
#include "sm/hb_utils.h"

static
sm_msg_messages
sm_register_transaction(sm_msg_struct * msg)
{
    // even in an exclusive mode we don't block ro-transactions
    if (xmGetExclusiveModeId() != -1 && !msg->data.trinfo.rdonly) {
        xmBlockSession(msg->sid);
        return msg_error; // come again later
    } else {
        msg->trid = get_transaction_id(msg->data.trinfo.rdonly);

        if (msg->trid != -1) {
            if (msg->data.trinfo.excl) {
                xmEnterExclusiveMode(msg->sid);
            }
        }

        return msg_ok;
    }
};

static
sm_msg_messages
sm_release_transaction(sm_msg_struct * msg)
{
    session_id excl_sid = xmGetExclusiveModeId();

    give_transaction_id(msg->trid, msg->data.trinfo.rdonly);

    if (msg->data.trinfo.rdonly) {
      // Read-only query has just finished; snapshot advancement might be possible
        if (UEventSet(&end_of_rotr_event, __sys_call_error) != 0) {
            throw SYSTEM_EXCEPTION("Event signaling for possibility of snapshot advancement failed");
        }
    } else {
      // Update query has just ended; check for need to advance snapshots or truncate the log
        if (llNeedCheckpoint()) {
            llActivateCheckpoint(); // maintenance checkpoint
        } else {
            if (UEventSet(&CheckpointEvent,  __sys_call_error) != 0) {
                throw SYSTEM_EXCEPTION("Event signaling for checking of snapshot advancement failed");
            }
        }

        if (msg->sid == excl_sid) {
            xmExitExclusiveMode();
        } else if (excl_sid != -1) {
          // updater ended and exclusive tr is waiting: try to start it
            xmTryToStartExclusive();
        }
    }

    return msg_ok;
}

static
sm_msg_messages
sm_lock_entity(sm_msg_struct * msg)
{
    msg->data.lock.result =
      lm_table.lock(msg->trid, msg->sid,
        resource_id(msg->data.lock.name, msg->data.lock.rkind),
          msg->data.lock.mode, LOCK_LONG, 0);

    if (msg->data.lock.result != LOCK_OK) {
        U_ASSERT(msg->data.lock.result == LOCK_NOT_LOCKED);

        if (lm_table.deadlock(msg->trid, true)) {
            msg->data.lock.result = LOCK_DEADLOCK;
            tr_lock_head* tr_head = tr_table.find_tr_lock_head(msg->trid);

            if (tr_head == NULL) {
                throw SYSTEM_EXCEPTION("Incorrect logic in SM's lock manager");
            }

            tr_head->tran->status = ROLLING_BACK_AFTER_DEADLOCK;
        };
    };

    return msg_ok;
};

int sm_server_handler(void *arg)
{
    sm_msg_struct *msg = (sm_msg_struct*)arg;
    
    try {
        GaintLockWarden gaintLock(true);

        switch (msg->cmd) {
        case msg_request_transaction_id:
            msg->cmd = sm_register_transaction(msg);
            break;
        case msg_release_transaction_id:
            msg->cmd = sm_release_transaction(msg);
            break;
        case msg_lock_object:
            sm_lock_entity(msg); /* NOTE: No return value */
            break;
        case msg_release_locks:
            lm_table.release_tr_locks(msg->trid);
            break;
        case msg_unlock_object:
            lm_table.unlock(msg->trid, resource_id(msg->data.lock.name, msg->data.lock.rkind));
            break;
  /* Former shutdown commands */
/*
        case 10:
        case 11:
            U_ASSERT(false);
            break;
*/
        case msg_register_session:
            bm_reset_io_statistics();
            bm_register_session(msg->sid, msg->data.reg.num);
            msg->data.reg.num = databaseOptions->bufferCount;
            msg->data.reg.mptr = mb->catalog_masterdata_block;
            msg->data.reg.transaction_flags = mb->transaction_flags;
            msg->data.reg.layer_size = mb->layer_size;
            msg->cmd = msg_ok;
            break;
        case msg_unregister_session:
            bm_unregister_session(msg->sid);
            msg->cmd = msg_ok;
            bm_log_out_io_statistics();
            break;
        case msg_alloc_data_block:
            WuAllocateDataBlockExn(msg->sid,
              &(msg->data.swap_data.ptr),
              &(msg->data.swap_data.offs),
              &(msg->data.swap_data.swapped));
            msg->cmd = msg_ok;
            break;
        case msg_alloc_tmp_block:
            WuAllocateTempBlockExn(msg->sid,
              &(msg->data.swap_data.ptr),
              &(msg->data.swap_data.offs),
              &(msg->data.swap_data.swapped));
            msg->cmd = msg_ok;
            break;
        case msg_release_block:
            WuDeleteBlockExn(msg->sid, *(xptr*)(&(msg->data.ptr)));
            msg->cmd = msg_ok;
            break;
        case msg_unswap_block:
            WuGetBlockExn(msg->sid,
              msg->data.swap_data.ptr,
              &(msg->data.swap_data.offs),
              &(msg->data.swap_data.swapped));
            msg->cmd = msg_ok;
            break;

        /* These are about xclusive modes. Do not know what they are. */
        case msg_enter_excl_mode:
            U_ASSERT(false);
            bm_enter_exclusive_mode(msg->sid, &(msg->data.reg.num));
            msg->cmd = msg_ok;
            break;
        case msg_exit_excl_mode:
            U_ASSERT(false);
            bm_exit_exclusive_mode(msg->sid);
            msg->cmd = msg_ok;
            break;
        case msg_lock_block:
            bm_memlock_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
            msg->cmd = msg_ok;
            break;
        case msg_unlock_block:
            bm_memunlock_block(msg->sid, *(xptr*)(&(msg->data.ptr)));
            msg->cmd = msg_ok;
            break;
        case msg_get_stats:
            bm_block_statistics(&(msg->data.stat));
            msg->cmd = msg_ok;
            break;
/*
        case 32:
        case 33: // WARNING: WTF???
            U_ASSERT(false);
            msg->cmd = msg_ok;
            break;
*/
        case msg_delete_tmp_blocks:
            bm_delete_tmp_blocks(msg->sid);
            msg->cmd = msg_ok;
            break;
        case msg_register_transaction:
            bm_register_transaction(msg->sid, msg->trid);

            try {
                WuOnRegisterTransactionExn(msg->sid, msg->data.want_snapshot, (TIMESTAMP*) &msg->data.snp_ts);
            } catch(ANY_SE_EXCEPTION) {
                bm_unregister_transaction(msg->sid, msg->trid);
                throw;
            }

            msg->cmd = msg_ok;
            break;
        case msg_unregister_transaction:
            WuOnUnregisterTransactionExn(msg->sid);
            bm_unregister_transaction(msg->sid, msg->trid);

            if (mb->catalog_masterdata_block != msg->data.ptr) {
                mb->catalog_masterdata_block = msg->data.ptr;
                flush_master_block();
            }

            msg->cmd = msg_ok;
            break;
        case msg_create_version:
            /* create version for the block */
            WuCreateBlockVersionExn(msg->sid,
                        msg->data.swap_data.ptr,
                        &(msg->data.swap_data.offs),
                        &(msg->data.swap_data.swapped));
            msg->cmd = msg_ok;
            break;
        case msg_transaction_finish_notification:
            if (msg->data.is_rollback) {
                WuOnRollbackTransactionExn(msg->sid);
            } else {
                WuOnCommitTransactionExn(msg->sid);
            }

            msg->cmd = msg_ok;
            break;
        case msg_hot_backup:
            /*
            * hot-backup request
            * important note: sm doesn't check consistency of requests. it presumes correct sequence of calls.
            * for now such checkings are performed in gov process, so we should be ok with this.
            */

            switch (msg->data.hb_struct.state) {
            case HB_START :
                msg->data.hb_struct.state =
                    hbProcessStartRequest(msg->data.hb_struct.state,
                        msg->data.hb_struct.is_checkp,
                        msg->data.hb_struct.incr_state);

            case HB_ARCHIVELOG:
                msg->data.hb_struct.state = hbProcessLogArchRequest(&(msg->data.hb_struct.lnumber));
            case HB_GETPREVLOG:
                msg->data.hb_struct.state = hbProcessGetPrevLogRequest(&(msg->data.hb_struct.lnumber));
            case HB_END:
                msg->data.hb_struct.state = hbProcessEndRequest();
            case HB_ERR:
                msg->data.hb_struct.state = hbProcessErrorRequest();
            default :
                msg->data.hb_struct.state = HB_ERR;
            }

            msg->cmd = msg_ok;
            break;
        default:
            throw SYSTEM_EXCEPTION("Unknown message in sm_vmm");
            msg->cmd = msg_error;
            break;
        }

        gaintLock.release();
    } catch (SednaUserException &e) {
        switch (e.get_code())
        {
        case SE1011:  // Data file has reached its maximum size.
            msg->cmd = msg_error_max_data_file_size;
            break;
        case SE1012:  // Temporary file has reached its maximum size.
            msg->cmd = msg_error_max_tmp_file_size;
            break;
        case SE1013:  // Cannot extend data file.
            msg->cmd = msg_error_extending_data_file;
            break;
        case SE1014:  // Cannot extend temporary file.
            msg->cmd = msg_error_extending_tmp_file;
            break;
        case SE1018:  // Transaction with this id already exists.
            msg->cmd = msg_error_transaction_exists;
            break;
        case SE1019:  // There is no transaction with this id.
            msg->cmd = msg_error_transaction_not_found;
            break;
        case SE1020:  // Transaction's limit on locked blocks in memory is exceeded.
            msg->cmd = msg_error_locked_blocks_limit;
            break;
        case SE1021:  // Cannot lock block in memory because it is not in memory.
            msg->cmd = msg_error_block_not_found;
            break;
        default    :  sedna_soft_fault(e, EL_SM);
        }
    } catch (SednaException &e) {
        sedna_soft_fault(e, EL_SM);
    } catch (ANY_SE_EXCEPTION) {
        sedna_soft_fault(EL_SM);
    }

    return 0;
}
