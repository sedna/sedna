#ifndef _SEDNA_MODULE_H_
#define _SEDNA_MODULE_H_

class SednaModule 
{
public:
    virtual void onSessionBegin() = 0;
    virtual void onSessionEnd() = 0;
    
    virtual void onTransactionBegin() = 0;
    virtual void onTransactionEnd() = 0;
};

#endif /* _SEDNA_MODULE_H_ */
