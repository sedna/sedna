/*
 * File:  context.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "context.h"

in_context::in_context()
{
;
}

in_context::in_context(enum in_status _in_st_)
{
  in_st = _in_st_;
}


out_context::out_context()
{
;
}

out_context::out_context(enum pos_fns _p_fns_)
{
  p_fns = _p_fns_;
}

out_context& out_context::operator= (out_context out_c)
{
  p_fns = out_c.p_fns;
  return *this;
}

bool out_context::operator> (out_context &out_c)
{
  if (p_fns > out_c.p_fns)
     return true;
  else
     return false;
}


out_context &max_out_context(out_context &_out_c1_, out_context &_out_c2_)
{
  if (_out_c1_.p_fns > _out_c2_.p_fns)
     return _out_c1_;
  else
     return _out_c2_;
}

