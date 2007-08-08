#ifdef _MSC_VER
#pragma once
#endif

#ifndef WUSTATETABLE_INCLUDED
#define WUSTATETABLE_INCLUDED

#include "wutypes.h"
#include "wuaux.h"
#include "wuincguard.h"

#ifdef DEBUG
#ifndef ST_MAX_COLUMNS_WITH_INFO
#define ST_MAX_COLUMNS_WITH_INFO 16
#endif
#else
#undef  ST_MAX_COLUMNS_WITH_INFO
#define ST_MAX_COLUMNS_WITH_INFO 1
#endif

struct StateTable
{
	int rowsCount;
	int columnsCount;
	void *mem;
	void *base;
	ptrdiff_t stride;
	size_t rowSize;
	size_t columnSize[ST_MAX_COLUMNS_WITH_INFO];
	void *columnBase[ST_MAX_COLUMNS_WITH_INFO];
};

void InitialiseStateTable(StateTable *stateTable);

int  ReserveStateTableColumn(StateTable *stateTable, TICKET *ticket, size_t size, int alignment);

int  CreateStateTableRows(StateTable *stateTable, int rowsCount, size_t maxRowSize);

void ResetStateTable(StateTable *stateTable);

void DeinitialiseStateTable(StateTable *stateTable);

int  GetStateTableCell(StateTable *stateTable, void **dest, TICKET ticket, int rowId);

int  GetStateTableMasterCell(StateTable *stateTable, void **dest, int rowId);

int  OccupyStateTableFirstVacantRow(StateTable *stateTable, int *rowId, int rngBegin, int rngEnd);

int  SetStateTableIsVacantRowFlag(StateTable *stateTable, int rowId, int isVacant);

int  IsStateTableRowVacant(StateTable *stateTable, int *isVacant, int rowId);

int  IsValidStateTableRowId(StateTable *stateTable, int rowId);

void DbgDumpStateTable(StateTable *stateTable);

void DbgDumpStateTableParams(StateTable *stateTable, DbgDumpMemoryParams *params);

#endif
