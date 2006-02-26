#include <stdafx.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <process.h>
#endif
#ifndef DTSFC_H
#include <dtsfc.h>
#endif
#ifdef _WIN32
#   include <oleauto.h>
#endif

#ifdef _WIN32_WCE
#   define NoExceptions
#endif

#ifdef NoExceptions
#   define try
#endif


#ifdef USE_DTSEARCH_NAMESPACE
using namespace dtSearch;
#endif

const char nl = '\n';



DOptions::DOptions() :
    AlphabetFile(alphabetFile, sizeof alphabetFile),
    StemmingRulesFile(stemmingRulesFile, sizeof stemmingRulesFile),
    NoiseWordFile(noiseWordFile, sizeof noiseWordFile),
    XmlIgnoreTags(xmlIgnoreTags, sizeof xmlIgnoreTags),
    BinaryFilterTextChars(binaryFilterTextChars, sizeof binaryFilterTextChars),
    SegmentationRulesFile(segmentationRulesFile, sizeof segmentationRulesFile),
    UserThesaurusFile(userThesaurusFile, sizeof userThesaurusFile),
    TextFieldsFile(textFieldsFile, sizeof textFieldsFile),
    HomeDir(homeDir, sizeof homeDir),
    PrivateDir(privateDir, sizeof privateDir),
    BooleanConnectors(booleanConnectors, sizeof booleanConnectors),
    FileTypeTableFile(fileTypeTableFile,sizeof fileTypeTableFile),
    MacroFile(macroFile, sizeof macroFile)
{   short result;
    dtssGetOptions(*this, result);
}

void DOptions::Save()
{   short result;
    dtssSetOptions(*this, result);
}

int DOptions::GetHyphenOption()
{   return hyphens;
}

void DOptions::SetHyphenOption(int v)
{   hyphens = v;
}

////////////////////////////////////////////////////////
//
//  DJobBase
//

const long JobId = 0x43211234L;
#ifndef FAIL
#   define FAIL -1
#   define SUCCESS 0
#endif

DJobBase::DJobBase(dtsJobBase& aBaseJob) :
    jobId(JobId),
    baseJob(aBaseJob),
    pParentJob(0)
{   initBase();
}

DJobBase *DJobBase::safeCast(void *pData)
{   if (pData) {
        DJobBase *j = (DJobBase *) pData;
        if (j->jobId == JobId)
            return j;
        }
    return 0;
}

DJobBase::~DJobBase()
{   jobId = 0;
}

int DJobBase::errorCB(dtsErrorInfo *pErrorInfo)
{	if (!pErrorInfo)
		return 0;

    DJobBase *job = DJobBase::safeCast(pErrorInfo->pUserData);
    int ret = 0;
    if (job)
		ret = job->onErrorBase(*pErrorInfo);
    return ret;
}

int DJobBase::onErrorBase(dtsErrorInfo& errorInfo)
{	int ret = 0;
    const char *msg = errorInfo.getMessage();
	int fQuestion = (msg && (msg[1] == 'Q'));
    if (fQuestion)
		ret = OnQuestion(errorInfo.getMessageCode(), errorInfo.getMessage(),
                   errorInfo.getMessageArg1(), errorInfo.getMessageArg2());
    else
        OnError(errorInfo.getMessageCode(), errorInfo.getMessage(),
                   errorInfo.getMessageArg1(), errorInfo.getMessageArg2());
    if (pParentJob)
		pParentJob->errorHandler.handleMessage(errorInfo.getMessageCode(), errorInfo.getMessage(),
                   errorInfo.getMessageArg1(), errorInfo.getMessageArg2());

	return ret;
}


int DJobBase::onReportBase(dtsMessage& msg)
{	int ret = dtsContinue;

    if (fShouldPumpMessages)
        PumpMessages();
    handleMessage(msg);
    if (pParentJob) {
		// To support nesting of multiple jobs inside a single DJobBase
		// Call DJobBase::SetParentJob() to make one job a parent of another.
		pParentJob->handleMessage(msg);
		if (pParentJob->fCancelImmediateRequested)
			fCancelImmediateRequested = pParentJob->fCancelImmediateRequested;
		if (pParentJob->fCancelRequested)
			fCancelRequested = pParentJob->fCancelRequested;
		}

    if (fCancelImmediateRequested)
        ret = dtsAbortImmediate;
    else if (fCancelRequested)
        ret = dtsAbort;

    return ret;
}

void DJobBase::SetParentJob(DJobBase& parent)
{	pParentJob = &parent;
}

int DJobBase::reportCB(void *pData, dtsMessage& msg)
{   // Ensures that user interface events, like button clicks,
    // and screen updates, get processed

    DJobBase *job = DJobBase::safeCast(pData);
	int ret = dtsContinue;
    if (job)
		ret = job->onReportBase(msg);
    return ret;
}

void DJobBase::initBase()
{
    baseJob.pReportCallBack = reportCB;
    baseJob.pReportData = this;
    baseJob.errorHandler = &errorHandler;
    errorHandler.pUserData = this;
    errorHandler.pNotifyFn = errorCB;
	pParentJob = 0;
    fRunning = false;
    fDone = false;
    fFailed = false;
    fCancelRequested = false;
    fCancelImmediateRequested = false;
    fShouldPumpMessages = true;
}

void DJobBase::Cancel()
{   fCancelRequested = true;
	baseJob.cancelFlag = dtsAbort;
#ifdef DLOGGER_H
	debugLog("DJobBase::Cancel()");
#endif

}

void DJobBase::CancelImmediate()
{   fCancelImmediateRequested = true;
	baseJob.cancelFlag = dtsAbortImmediate;
#ifdef DLOGGER_H
	debugLog("DJobBase::CancelImmediate()");
#endif
}

void DJobBase::beginExecution()
{   fCancelRequested = false;
    fCancelImmediateRequested = false;
    fDone = false;
    fRunning = true;
    errorHandler.clear();
    baseJob.cancelFlag = 0;
}

void DJobBase::endExecution(int aFailed)
{   fRunning = false;
    fDone = true;
    fFailed = aFailed;
}

int DJobBase::IsRunning()
{   return fRunning;
}

// return false when PumpMessage() returns false, which indicates
// WM_QUIT received
int DJobBase::PumpMessages()
{
#if defined(MfcAvailable)
    MSG msg;
    while (::PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE))
        if (!AfxGetThread ()->PumpMessage ())
			return false;
#endif
	return true;
}

void DJobBase::FormatErrors(DString& dest)
{	FormatErrors(dest, &errorHandler);
}

void DJobBase::FormatErrors(DString& dest, dtsErrorInfo *errors)
{	 if (!errors)
		return;
	for (int i = 0; i < errors->getCount(); ++i) {
    	if (dest.getLength())
        	dest << "\r\n";
        dest << errors->getMessage(i);
        }
}


/////////////////////////////////////////////////////////////
//
//  DSearchResults
//

DSearchResults::DSearchResults(long aMaxCount, long aFlags) :
    dtsSearchResults(aMaxCount, aFlags),
    iDoc(-1),
    temp(32000),
    DocHitCount(0),
    bHaveDocInfoStrings(false),
    bHaveUserFields(false)
{   init();
    whenCreated = clock();
}

void DSearchResults::init()
{   DocHitCount = 0;
    if (handle) {
        getSearchRequest(temp.str(), temp.getMaxLength());
        Request.setU8(temp);
        fGotCounts = 0;
        getCounts();
        }
    iDoc = -1;
    bHaveDocInfoStrings = false;
    bHaveUserFields = false;
    whenCreated = clock();
};

DSearchResults::DSearchResults(const DSearchResults& other) :
    dtsSearchResults(other),
    temp(32000),
    iDoc(-1),
    DocHitCount(0)
{   Request = other.Request;
    whenCreated = clock();
}

DSearchResults& DSearchResults::operator =(const DSearchResults& other)
{   dtsSR_Free(handle);
    handle = dtsSR_Copy(other.handle);
    init();
    return *this;
}

void DSearchResults::MakePdfWebHighlightFile(DString& dest)
{   dtsStringHandle h = makePdfWebHighlightFile(iDoc);
    dest.getFromStringHandle(h, true);
}

int DSearchResults::getNthDoc(long n)
{   if (iDoc == n)
		return SUCCESS;

	DocInfo.clear();

    DocInfoString.clear();
    DocName.clear();
    DocTitle.clear();
    DocInfoItems.clear();
    DocDisplayName.clear();
    iDoc = -1;
    if ((n < 0) || (n >= getCount()))
        return -1;

    if (dtsSearchResults::getDocInfo(n, DocInfo) < 0)
        return FAIL;

	bHaveDocInfoStrings = false;
	bHaveUserFields = false;
    iDoc = n;
	
    DocName.setU8(DocInfo.filename);
    DocTitle.setU8(DocInfo.title);
    DocDisplayName.setU8(DocInfo.displayName);
    DocHitCount = DocInfo.hitCount;


    return SUCCESS;
}

void DSearchResults::initDocInfoStrings()
{	if (bHaveDocInfoStrings)
		return;

    // Build a string set property from the DocInfoStrings
    int size = getDocInfoStrings(iDoc, temp.str(), temp.getMaxLength());
    if (size >= temp.getMaxLength()) {
        temp.extend(size);
        getDocInfoStrings(iDoc, temp.str(), temp.getMaxLength());
        }

    for(const char *p = temp.str(); p && *p; p += (strlen(p) + 1))
        DocInfoItems.append(p);

    // Set up other doc properties
    temp.clear();
    for (int i = 0; i < DocInfoItems.getCount(); i += 2) {
        temp << DocInfoItems.getString(i) << " = " << DocInfoItems.getString(i+1) << "\r\n";
        }
    DocInfoString.setU8(temp);
    bHaveDocInfoStrings = true;
}    

void DSearchResults::initUserFields()
{
	if (!bHaveUserFields) {
		userFields.copyFromBuf(DocInfo.userFields);
		bHaveUserFields = false;
		}
}

// Returns an item from the list of named properties associated with this document.
// There are two categories of named properties:
// - Built-in properties, which have names beginning with an underscore, such as _filename
// - Stored Fields, which have names that do not begin with an underscore
//
// To access StoredFields, the lazy-initialized storedFields string set is created from
// DocInfo.userFields and then used to look up the field name
// To access the built-in named properties, the lazy-initialized DocInfoItems is created
// and used to look up the property.
//
int DSearchResults::getDocInfoItem(const char *fieldName, DString& dest) {
	if (*fieldName == '_') {
		// built-in field names begin with underscore
		initDocInfoStrings();
		long i = DocInfoItems.FindFieldNameI(fieldName);
		if (i > -1) {
			DocInfoItems.getString(i+1, dest);
			return SUCCESS;
			}
		else
			return FAIL;
		}
	
	initUserFields();		
	int i = userFields.findFieldName(fieldName);
	if (i > FAIL) {
		dest = userFields.getString(i+1);
		return SUCCESS;
		}
	return FAIL;
	}


// Get a display name for a search results item, taking into account the PDF and HTML title-as-name options
const char *DSearchResults::getDisplayName(dtsSearchResultsItem& item, int fPdfTitleAsName, int fHtmlTitleAsName)
{   return item.getDisplayName(fPdfTitleAsName, fHtmlTitleAsName);
}

void DSearchResults::UrlEncodeItem(dtsSearchResultsItem& item, DString& dest, int maxLen)
{
    DFieldSet fields;
    fields.addField("DocId", item.docId);
    fields.addField("Index", item.indexRetrievedFrom);
    fields.addField("HitCount", item.originalHitCount);
    fields.urlEncode(dest);
    dest << "&hits=";
    for (int i = item.hitCount-1; (i >= 0); --i) {
        char hexHit[10];
        sprintf(hexHit, "%lx", item.hits[i]);
        if ((maxLen >= 0) && (dest.getLength() + strlen(hexHit) + 1 >= maxLen))
			break;
        dest << hexHit << '+';
    }
}

void DSearchResults::UrlDecodeItem(const char *url, DString& indexPath, long& docId,
		DArrayAsVector<long>& hitsArray)
{
    DFieldSet vars;
    vars.urlDecode(url);
    docId = vars.getIntField("DocId");
    const char *hits = vars.getField("Hits");
    const char *ix = vars.getField("Index");
    if (strIsBlank(ix))
        ix = "";
    indexPath = ix;

    DStringSet hitsSet;
    DString hitsCopy = hits;
    // There should be a space in the URL after each hit,
    // so look for the last space because otherwise we'll
    // get a random hit from a truncated number
    int iLastSpace = hitsCopy.rfind(' ');
    if (iLastSpace > 0)
        hitsCopy.truncate(iLastSpace);
    hitsSet.tokenize(hitsCopy, ' ');

	hitsArray.clear();

    for (int i = 0; i < hitsSet.getCount(); ++i)
        hitsArray.append(strtol(hitsSet.getString(i), 0, 16));
}


/////////////////////////////////////////////////////////////
//
//  DSearchJob

DSearchJob::DSearchJob() :
    DJobBase(searchJob),
    Request(&searchJob.request2),
    FileConditions(&searchJob.fileConditions2),
    BooleanConditions(&searchJob.booleanConditions),
    IncludeFilters(&searchJob.filesToSearch.includeFilters),
    ExcludeFilters(&searchJob.filesToSearch.excludeFilters),
    FoldersToSearch(&searchJob.filesToSearch.dirList),
    IndexesToSearch(&searchJob.indexesToSearch),
    ListingName(searchJob.output.listingName, sizeof searchJob.output.listingName),
    results(0),
    dataSourceToDelete(0),
    fWantResultsAsFilter(0)
{
    Clear();
    SetMaxFiles(100);
}

DSearchJob::~DSearchJob()
{   if (dataSourceToDelete)
        delete dataSourceToDelete;
    dataSourceToDelete = 0;
    if (results)
        delete results;
    results = 0;
}

void DSearchJob::Clear()
{   searchJob.clear();
    IncludeFilters.clear();
    ExcludeFilters.clear();
    FoldersToSearch.clear();
    IndexesToSearch.clear();
    FileConditions.clear();
    BooleanConditions.clear();
    ListingName.clear();
    Request.clear();
    AttachDataSource(0, 0);
    fDelayDocInfo = false;
    initBase();
}

void DSearchJob::handleMessage(dtsMessage& msg)
{
    switch(msg.command) {
        case dtsnCheckForAbort:
            OnCheckAbort();
            break;
        case dtsnSearchFound: {
            long hitsInFile = msg.paramA;
            const char *name = msg.strParam;
            dtsSearchResultsItem *item = (dtsSearchResultsItem *) msg.paramB;
            totalFiles++;
            totalHits+= hitsInFile;
            bVetoThisItem = false;
            OnFound(totalFiles, totalHits, name, hitsInFile, *item);
            if (bVetoThisItem)
				msg.result = dtsVetoSearchResultsItem;
            break;
            }
        case dtsnSearchWhere: {
            if (msg.paramA == 0)
                OnSearchingIndex(msg.strParam);
            else
                OnSearchingFile(msg.strParam, msg.paramA, msg.paramB);
            break;
            }
        default:
            break;
        }
}

// searchJob.searchFlags2 (32-bits) replaces the old
// searchJob.searchFlags, which was only 16 bits.  To ensure backward compatibility,
// set/clear both flags in the access functions
void DSearchJob::SetSearchFlag(long flag, int value)
{   if (value)
        searchJob.searchFlags2 |= flag;
    else
        searchJob.searchFlags2 &= (~flag);
    searchJob.searchFlags = (short) (searchJob.searchFlags2 & 0xffff);
}

void DSearchJob::ClearSearchFlags()
{   searchJob.searchFlags2 = 0;
    searchJob.searchFlags = 0;
}

void DSearchJob::SetFuzzySearching(int fuzziness)
{   SetSearchFlag(dtsSearchFuzzy, (fuzziness > 0));
    searchJob.fuzziness = fuzziness;
}

void DSearchJob::SetPhonicSearching(int v)
{   SetSearchFlag(dtsSearchPhonic, v);
}

void DSearchJob::SetStemming(int v)
{   SetSearchFlag(dtsSearchStemming, v);
}

void DSearchJob::SetMaxFiles(long aMaxFiles)
{   // For backward compatibility
    if (aMaxFiles < 0x7fff)
        searchJob.maxFilesToRetrieve = (short) aMaxFiles;
    else
        searchJob.maxFilesToRetrieve = 0;
    // Version 6.12 and later will check for this 32-bit value
    searchJob.maxFilesToRetrieve2 = aMaxFiles;
}

long DSearchJob::GetMaxFiles()
{   if (searchJob.maxFilesToRetrieve2)
        return searchJob.maxFilesToRetrieve2;
    else
        return searchJob.maxFilesToRetrieve;
}

void DSearchJob::SetSynonymSearching(int v, int fWordNet, int fUserSynonyms, int fWordNetRelated)
{   SetSearchFlag(dtsSearchSynonyms, v);
    SetSearchFlag(dtsSearchWordNetSynonyms, fWordNet);
    SetSearchFlag(dtsSearchWordNetRelated, fWordNetRelated);
    SetSearchFlag(dtsSearchUserSynonyms, fUserSynonyms);
}

void DSearchJob::SetNaturalLanguage(int v)
{   SetSearchFlag(dtsSearchNatural, v);
}

void DSearchJob::allocResults()
{   delete results;
    results = new DSearchResults(GetMaxFiles(), srWantFileMap);
    searchJob.resultsHandle = results->getHandle();

    if (fWantResultsAsFilter) {
        outputSearchFilter.attach(new dtsSearchFilter);
        searchJob.outputSearchFilterHandle = outputSearchFilter->getHandle();
        for (int i = 0; i < IndexesToSearch.getCount(); ++i)
            outputSearchFilter->addIndex(IndexesToSearch.getString(i));
        }
    else
        searchJob.outputSearchFilterHandle = 0;
}


void DSearchJob::Execute()
{
    beginExecution();

    searchJob.action.searchFiles =  ((FoldersToSearch.getCount() > 0) || searchJob.dataSourceToSearch);
    searchJob.action.searchIndexes = (IndexesToSearch.getCount() > 0);

    allocResults();

    totalFiles = 0;
    totalHits = 0;

    // This tells the dtSearch Engine to delay reading document records until
    // they are requested from the dtsSearchResults.  Otherwise, the records will
    // be read each time a document is retrieved.  If 5000 documents are retrieved
    // but search results are limited to the 100 best-matching files, and if
    // this optimization is not done, then 4900 document records will be read
    // unnecessarily.  Reading a document record from disk takes about 0.005 seconds.
    //
    // The disadvantage of this optimization is that the OnFound() callback notification
    // will not get the filename of each file as it is retrieved.
    //
    // If an index contains any obsolete document records, either because a document
    // was indexed more than once (it was indexed, modified, and then reindexed) or because
    // documents were removed from the index, then dtsSearchDelayDocInfo is ignored when
    // searching that index.
    if (fDelayDocInfo)
        SetSearchFlag(dtsSearchDelayDocInfo, true);


    short failFlag;
    dtssDoSearchJob(searchJob, failFlag);

    if (results)
        results->updateFromSearch();

    endExecution(failFlag);
}


DIndexMergeJob::DIndexMergeJob() :
    DJobBase(mergeJob),
    IndexesToMerge(&mergeJob.indexesToMerge)
{   Clear();
}

void DIndexMergeJob::Clear()
{   IndexesToMerge.clear();
    mergeJob.iTargetIndex = 0;
    mergeJob.fAllowConcurrentAccess = true;
    mergeJob.fClearTarget = true;
    initBase();
}

void DIndexMergeJob::SetTarget(int iTarget, int fShouldClear)
{   mergeJob.iTargetIndex = iTarget;
    mergeJob.fClearTarget = fShouldClear;
}

void DIndexMergeJob::Execute()
{   beginExecution();

    long result;
    dtssMergeIndexes(mergeJob);
    result = mergeJob.result;

    endExecution(result);
}

void DIndexMergeJob::handleMessage(dtsMessage& msg)
{
    switch(msg.command) {
        case dtsnCheckForAbort:
            OnCheckAbort();
            break;
        default: {
            dtsMergeStatusInfo *prog = (dtsMergeStatusInfo *) msg.ptrParam;
            if (prog)
                OnProgressUpdate(*prog);
            break;
            }
        }
}

DIndexVerifyJob::DIndexVerifyJob() :
    DJobBase(verifyJob),
    IndexPath(&verifyJob.indexPath)
{   Clear();
}

void DIndexVerifyJob::Clear()
{   IndexPath.clear();
    initBase();
}

void DIndexVerifyJob::Execute()
{   beginExecution();

    long result;
    dtssVerifyIndex(verifyJob);
    result = verifyJob.result;

    endExecution(result);
}

void DIndexVerifyJob::handleMessage(dtsMessage& msg)
{
    switch(msg.command) {
        case dtsnCheckForAbort:
            OnCheckAbort();
            break;
        default: {
            dtsVerifyStatusInfo *prog = (dtsVerifyStatusInfo *) msg.ptrParam;
            if (prog)
                OnProgressUpdate(*prog);
            break;
            }
        }
}


/////////////////////////////////////////////////////////////
//
//  DIndexJob
//

DIndexJob::DIndexJob() :
    DJobBase(indexJob),
    IndexName(indexJob.indexName, sizeof indexJob.indexName),
    IndexPath(indexJob.indexPath, sizeof indexJob.indexPath),
    IncludeFilters(&indexJob.toAdd.includeFilters),
    ExcludeFilters(&indexJob.toAdd.excludeFilters),
    FoldersToIndex(&indexJob.toAdd.dirList),
    ToRemoveListName(indexJob.toRemoveList, sizeof indexJob.toRemoveList),
    FileListName(indexJob.toAdd.fileListName, sizeof indexJob.toAdd.fileListName),
    DirListName(indexJob.toAdd.dirListName, sizeof indexJob.toAdd.dirListName),
    StoredFields(&indexJob.storedFields)
{
    Clear();
}

DIndexJob::~DIndexJob()
{
}

void DIndexJob::Clear()
{   indexJob.clear();
    IndexName.clear();
    IndexPath.clear();
    IncludeFilters.clear();
    ExcludeFilters.clear();
    FoldersToIndex.clear();
    ToRemoveListName.clear();
    FileListName.clear();
    DirListName.clear();
    AttachDataSource(0, 0);
    indexJob.createFlags.relativePaths = true;
    indexJob.allowConcurrentAccess = true;
    initBase();
}

void DIndexJob::handleMessage(dtsMessage& msg)
{
    switch(msg.command) {
        case dtsnCheckForAbort:
            OnCheckAbort();
            break;
        default: {
            dtsIndexProgressInfo *prog = (dtsIndexProgressInfo *) msg.ptrParam;
            if (prog)
                OnProgressUpdate(*prog);
            break;
            }
        }
}

void DIndexJob::Execute()
{
    beginExecution();

    if (!DataSourceToIndex)
        indexJob.dataSourceToIndex = 0;
    else
        indexJob.dataSourceToIndex = DataSourceToIndex->getInterface();

    short result;
    dtssDoIndexJob(indexJob, result);

    endExecution(result);
}

void DIndexJob::Cancel()
{   if (!DataSourceToIndex.isEmpty())
        DataSourceToIndex->abort();
    DJobBase::Cancel();

}

void DIndexJob::CancelImmediate()
{   if (!DataSourceToIndex.isEmpty())
        DataSourceToIndex->abort();
    DJobBase::CancelImmediate();
}

/////////////////////////////////////////////////////////////
//
//  DOutputBase -- base for DFileConvertJob and DSearchReportJob
//

DOutputBase::DOutputBase(dtsJobBase& baseJob) :
    DJobBase(baseJob),
    outputDestination(OutputToString),
    outputStringMaxLen(-1)
{
	outputInfo.clear();

	HtmlHead.setPVal(&outputInfo.htmlHead);
    HtmlBaseHref.setPVal(&outputInfo.baseHref);
    BeforeHit.setPVal(&outputInfo.beforeHit);
    AfterHit.setPVal(&outputInfo.afterHit);
    Header.setPVal(&outputInfo.header);
    Footer.setPVal(&outputInfo.footer);
    OutputFile.setPVal(&outputInfo.outputFile);
	Clear();
    outputInfo.outputFormat = it_HTML;
}

void DOutputBase::Clear()
{   outputInfo.clear();
    initBase();
}

void DOutputBase::linkOutputBuffers()
{   // OutputFile is exposed as a property so it might have been changed
    if (!OutputFile.isBlank())
        outputDestination = OutputToFile;

    switch(outputDestination) {
        case OutputToFile:

            break;
        case OutputToString:
        case OutputToCallback:
        default:
            outputInfo.pOutputFn = outputCB;
            outputInfo.pData = this;
            break;
        }

    // If output is to it_Ansi (i.e., 8-bit text) and is directed to a
    // string, the string has to be encoded inside the DStringProperty
    // as UTF-8, so use that format instead of Ansi -- this avoids a lot
    // of unnecessary conversion inside the dtSearch Engine between
    // Ansi and UTF-8.  The caller can still get Ansi text by calling
    // OutputString.getA().
    if ((outputInfo.outputFormat == it_Ansi) && (outputDestination != OutputToFile))
        outputInfo.outputFormat = it_Utf8;
}

#ifdef _WIN32
//
//  Converting the outputString, which is a UTF-8 encoded character buffer,
//  into Unicode text for a BSTR requires special handing for RTF and HTML
//  output.  For these formats, it is *not* correct to convert the UTF-8
//  encoded characters into Unicode, because HTML and RTF output strings
//  are expected to handle encoding using 8-bit characters.  For example,
//  a VBScript function might be calling this to get HTML to use for
//  Response.Write.  The HTML will have a charset=utf-8 in the header,
//  so what Response.Write outputs must be UTF-8 characters.  Response.Write
//  will convert the Unicode in its string argument to Ansi.
//  To get the correct output, therefore, we treat the UTF-8 encoded
//  HTML (or RTF) as Ansi, convert it to Unicode, and then Response.Write
//  will reverse the conversion.
//
BSTR DOutputBase::GetOutputBSTR()
{   if ((outputInfo.outputFormat == it_Utf8) || (outputInfo.outputFormat == it_Ansi))
        return outputString.allocSysString();

    DWString ws;
    ws.storeAnsiAsUnicode(outputString);
    BSTR bstr = ::SysAllocStringLen(ws, ws.getLength());
    return bstr;
}
#endif


void DOutputBase::SetOutputToCallback()
{   outputDestination = OutputToCallback;
    linkOutputBuffers();
}

void DOutputBase::SetOutputToString(long aMaxLen)
{   outputStringMaxLen = aMaxLen;
    outputDestination = OutputToString;
    linkOutputBuffers();
}

void DOutputBase::SetOutputFormat(int fmtId)
{   outputInfo.outputFormat = fmtId;
}

void DOutputBase::writeToString(const char *text, int len)
{   if ((outputStringMaxLen > 0) && (len + outputString.getLength() >= outputStringMaxLen)) {
        len = outputStringMaxLen - outputString.getLength() - 1;
        Cancel();
#ifdef DLOGGER_H
		debugLog("Cancelled because of output string length ", outputString.getLength(), outputStringMaxLen);
#endif
        }
    outputString.store(text, len);
}

int DOutputBase::outputCB(void *pData, const char *text, int len)
{   DOutputBase *j = DOutputBase::safeCast(pData);
    if (j) {
        if (j->outputDestination == OutputToString)
            j->writeToString(text, len);
        j->OnOutput(text, len);
        }
    return 0;
}


DOutputBase *DOutputBase::safeCast(void *pData)
{   if (!DJobBase::safeCast(pData))
        return 0;

    DOutputBase *r = (DOutputBase *) pData;
    return r;
}

void DOutputBase::handleMessage(dtsMessage& msg)
{
    switch(msg.command) {
        case dtsnConvertPercentDone:
            OnPercentDone(msg.paramA);
            break;
        default:
            break;
        }
}


/////////////////////////////////////////////////////////////
//
//  DFileConvertJob
//

DFileConvertJob::DFileConvertJob() :
    DOutputBase(convertJob),
    InputFile(&convertJob.inputFile),
    AlphabetLocation(&convertJob.alphabetLocation),
    IndexRetrievedFrom(&convertJob.indexRetrievedFrom),
    inputType(InputUndefined),
    inputFlags(0)
{   initBase();
    linkBuffers();
    convertJob.flags |= dtsConvertSkipHiddenHits;
}

DFileConvertJob::~DFileConvertJob()
{
}

void DFileConvertJob::SetHits(long *aHits, long ct)
{   hits.clear();
    for (long i = 0; i < ct; ++i)
        hits.append(aHits[i]);
    linkBuffers();
}

void DFileConvertJob::SetInputText(const char *s, const char *aFn, int aTypeId)
{   dtsFileInfo info;
    if (aFn)
        strCopy(info.filename, aFn, sizeof info.filename);
    inputText.setFileInfo(info);
    inputText.setTextData(s);
    if (!aTypeId)
		aTypeId = it_Utf8;
		
	convertJob.typeId = aTypeId;
	
    inputType = InputFromString;
    linkBuffers();
}

void DFileConvertJob::Execute()
{   beginExecution();
    linkBuffers();
    convertJob.errorHandler = &errorHandler;

    short result;
    dtssConvertFile2(convertJob, result);
    endExecution(result);
}

void DFileConvertJob::linkBuffers()
{
    linkOutputBuffers();
    convertJob.output = &outputInfo;
    convertJob.hits = hits.c_vector();
    convertJob.hitCount = hits.getCount();

    // Input from a file convert job can either come from a filename
    // (inputFile) or from a dtsInputStream (inputSource), a callback-driven
    // source of text data.
    convertJob.inputSource = 0;
    convertJob.inputFile = 0;

    if (!InputFile.isBlank())
        inputType = InputFromFile;
    switch(inputType) {
        case InputFromString:
            convertJob.inputSource = inputText.getInterface();
            convertJob.inputSource->flags |= inputFlags;
			if (!convertJob.typeId)
				convertJob.typeId = it_Utf8;
            convertJob.inputSource->typeId = convertJob.typeId;
            break;
        case InputFromISource:
            convertJob.inputSource = &inputSource;
            convertJob.inputSource->flags |= inputFlags;
            break;
        case InputFromFile:
            convertJob.inputFile = InputFile.str();
            break;
        default:
            break;
        }
}

void DFileConvertJob::SetInputItem(DSearchResults& res, long iItem)
{   dtsSearchResultsItem item;
    int v = FAIL;
    if (iItem < 0)
        iItem = res.getSelectedDoc();
    v = res.getDocInfo(iItem, item);
    if (v == FAIL)
        return;
    SetInputItem(item);
}

void DFileConvertJob::SetInputItem(const dtsSearchResultsItem& item)
{   AlphabetLocation.set(item.indexRetrievedFrom);
    IndexRetrievedFrom.set(item.indexRetrievedFrom);
    convertJob.docId = item.docId;

    InputFile.set(item.filename);
    inputType = InputFromFile;
    SetHits(item.hits, item.hitCount);
    if (item.parserId)
		convertJob.typeId = item.parserId;
	else
		convertJob.typeId = item.typeId;

    if ((item.typeId == it_HTML) || (item.typeId == it_OutlookExpressMessage) ||
        (item.typeId == it_EudoraMessage) || (item.typeId == it_MimeMessage) ||
        (item.typeId == it_DatabaseRecord2))
        SetFlags(dtsConvertInputIsHtml);
    else
        SetFlags(dtsConvertInputIsNotHtml);

	if (item.cachedOriginalPos64)
		SetFlags(dtsConvertGetFromCache);

}

/////////////////////////////////////////////////////////////
//
//  DSearchReportJob
//

DSearchReportJob::DSearchReportJob(dtsSearchResults& aResults) :
    DOutputBase(reportJob),
    selections(aResults.getCount()),
    ContextHeader(&reportJob.contextHeader),
    ContextFooter(&reportJob.contextFooter),
    ContextSeparator(&reportJob.contextSeparator),
    FileHeader(&reportJob.fileHeader),
    FileFooter(&reportJob.fileFooter),
    pResults(0)
{   initBase();
    linkBuffers();

    AttachResults(&aResults, true);
}

DSearchReportJob::DSearchReportJob():
    DOutputBase(reportJob),
    selections(1),
    ContextHeader(&reportJob.contextHeader),
    ContextFooter(&reportJob.contextFooter),
    ContextSeparator(&reportJob.contextSeparator),
    FileHeader(&reportJob.fileHeader),
    FileFooter(&reportJob.fileFooter),
    pResults(0),
    fShouldDeleteResults(0)
{   initBase();
    linkBuffers();
}

DSearchReportJob::~DSearchReportJob()
{   AttachResults(0);
}

void DSearchReportJob::AttachResults(dtsSearchResults *pRes, int fMakePrivateCopy)
{   if (pResults) {
        if (fShouldDeleteResults)
            delete pResults;
        pResults = 0;
        fShouldDeleteResults = false;
        }

    if (pRes) {
        if (fMakePrivateCopy) {
            pResults = new dtsSearchResults(*pRes);
            fShouldDeleteResults = true;
            }
        else {
            pResults = pRes;
            fShouldDeleteResults = false;
            }
        }
    if (pResults)
        selections.alloc(pResults->getCount());
}

void DSearchReportJob::Execute()
{   beginExecution();
    outputString.clear();

    linkBuffers();

    // Convert selections from a bit vector to an array of items
    DArrayAsVector<long> selectList;
    if (!(reportJob.flags & dtsReportIncludeAll)) {
        for (long i = 0; i < pResults->getCount(); ++i) {
            if (selections.checkBit(i))
                selectList.append(i);
            }
        reportJob.selections = selectList.c_vector();
        reportJob.selectCount = selectList.getCount();
        }

    pResults->makeReport(reportJob);

    // clean up dangling reference
    reportJob.selections = 0;

    endExecution(0);
}

void DSearchReportJob::linkBuffers()
{
    linkOutputBuffers();
    reportJob.output = &outputInfo;
}

static const int ContextFlags = (dtsReportByParagraph | dtsReportByWord | dtsReportWholeFile);

void DSearchReportJob::SetContextWords(int n, int fExact)
{   reportJob.flags &= (~ContextFlags);
    reportJob.flags |= dtsReportByWord;
    if (fExact)
        reportJob.flags |= dtsReportByWordExact;
    reportJob.amountOfContext = n;
}

void DSearchReportJob::SetContextWholeFile()
{   reportJob.flags &= (~ContextFlags);
    reportJob.flags |= dtsReportWholeFile;
}

void DSearchReportJob::SetContextParagraphs(int n)
{   reportJob.flags &= (~ContextFlags);
    reportJob.flags |= dtsReportByParagraph;
    reportJob.amountOfContext = n;
}

void DSearchReportJob::SelectAll()
{   reportJob.flags |= dtsReportIncludeAll;
}

void DSearchReportJob::ClearSelections()
{   reportJob.flags &= (~dtsReportIncludeAll);
    selections.clearAll();
}

void DSearchReportJob::SelectRange(long iFirst, long iLast)
{   reportJob.flags &= (~dtsReportIncludeAll);
    for (long i = iFirst; i <= iLast; ++i)
        selections.setBit(i);
}

DConvertPathRequest::DConvertPathRequest() :
    Path(&m_request.path),
    TableRegistryKey(&m_request.registryKey)
{
}

void DConvertPathRequest::RefreshMappings()
{   m_request.flags = (dtsCpRefreshMappings | dtsCpStoreInRegistry);
    executeRequest();
}

void DConvertPathRequest::MakeLocal(long siteId, bool readFromRegistry)
{   m_request.flags = dtsCpMakeLocal;
	if (readFromRegistry)
		m_request.flags |= dtsCpReadFromRegistry;
    m_request.siteId = siteId;
    executeRequest();
}

void DConvertPathRequest::MakeVirtual(long siteId, bool readFromRegistry)
{   m_request.flags = dtsCpMakeVirtual;
	if (readFromRegistry)
		m_request.flags |= dtsCpReadFromRegistry;
    m_request.siteId = siteId;
    executeRequest();
}

void DConvertPathRequest::MakeUNC()
{   m_request.flags = dtsCpMakeUNC;
    executeRequest();
}

#ifndef MAX_PATH
#   define MAX_PATH 512
#endif

void DConvertPathRequest::executeRequest()
{   char result[MAX_PATH];
    m_request.dest = result;
    m_request.maxLen = MAX_PATH;
    m_request.errorHandler = 0;
    strcpy(result, "");
    dtssConvertPath2(m_request);
    if ((m_request.flags & (dtsCpMakeLocal | dtsCpMakeVirtual | dtsCpMakeUNC)) ||
        strlen(result))
        ConvertedPath.setA(result);
}


DListIndexJob::DListIndexJob() :
    DJobBase(m_listJob),
    ToMatch(&m_listJob.toMatch),
    IndexPath(&m_listJob.indexPath),
    OutputFile(&m_listJob.outputFile)
{	initBase();
}

void DListIndexJob::SetOutputToString()
{   OutputFile.clear();
}

void DListIndexJob::Execute()
{   short failFlag = 0;
    beginExecution();

    if (OutputFile.isBlank())
        m_listJob.listFlags |= dtsListIndexReturnString;
    else
        m_listJob.listFlags &= (~dtsListIndexReturnString);
    dtssListIndex(m_listJob, failFlag);

    if (m_listJob.outputString) {
        OutputString.getFromStringHandle(m_listJob.outputString, true);
        m_listJob.outputString = 0;
        }

    endExecution(failFlag);
}

const long ValidJobThread = 0x76675445L;

DJobThread::DJobThread(DJobBase& aJob) :
    job(aJob),
    fAbort(0),
    fDone(0),
    fRunning(0),
    validId(ValidJobThread),
    hThread(0)
{
}

DJobThread::~DJobThread()
{
    validId = 0;
}

DJobThread *DJobThread::SafeCast(void *p)
{    DJobThread *cj = (DJobThread *) p;
    if (cj && (cj->validId == ValidJobThread))
        return cj;
    else
        return 0;
}

void DJobThread::abort(int type)
{    if (!fDone)    {
        fAbort = type;
        if (type == dtsAbortImmediate)
            job.CancelImmediate();
        else if (type == dtsAbort)
            job.Cancel();
#ifdef DLOGGER_H
		debugLog("Job thread aborted");
#endif
        }
}

//  abort() is preferred because this halts the thread in an unpredictable state
void DJobThread::terminate()
{
#if defined(_WIN32)
    _endthread();
#endif
    fDone = true;
    fAbort = true;
}

void DJobThread::start(int priority)
{
#if defined(_WIN32)
     hThread = (HANDLE) _beginthread(startCB, 0, (void *) this);
    if (priority != THREAD_PRIORITY_NORMAL)
    	SetThreadPriority(hThread, priority);
#else
    startCB(this);
#endif
}

void DJobThread::startCB(void *pData)
{
    DJobThread *cj = SafeCast(pData);
    if (cj) {
        cj->fRunning = true;
        cj->fDone = false;
        cj->job.Execute();
        cj->hThread = 0;
        cj->fDone = true;
        cj->fRunning = false;
        }
    // Thread will terminate
}

static void addDateProperty(DStringSet& ss, const char *name, dtsFileDate& d)
{	DFileDate date;
	date.copy(d);
	char image[80];
	sprintf(image, "M%2.2d/D%2.2d/Y%4.4d %2.2d:%2.2d:%2.2d", date.month, date.day, date.year,
		date.hour, date.minute, date.second);
	ss.append(name);
	ss.append(image);
	DString local;
	date.formatLocal(local);

    DString name2 = name;
    name2 << "Local";

    ss.append(name2);
    ss.append(local);
}


void DIndexInfo::GetAsStrings(DStringSet& ss)
{	ss.clear();
	ss.append("DocCount");
	ss.append(docCount);
	ss.append("WordCount");
    ss.append(wordCount);
    ss.append("Size");
    __int64 sizeK = (indexSize64/1024);
    ss.append((long) sizeK);
    ss.append("PercentFull");
    ss.append(percentFull);
    addDateProperty(ss, "DateCreated", created);
    addDateProperty(ss, "DateUpdated", updated);
    addDateProperty(ss, "DateCompressed", compressed);
}

DCrashHandlerInfo::DCrashHandlerInfo() :
	LogName(&logName2),
	MiniDumpName(&miniDumpName)
{ }

DCrashHandlerInfo& DCrashHandlerInfo::operator=(const dtsCrashHandlerInfo& other) {
	copy(other);
	return *this;
}

void DCrashHandlerInfo::copy(const dtsCrashHandlerInfo& other)
{	dtsCrashHandlerInfo::copy(other);
	if (strIsBlank(other.logName2))
		LogName.setU8(other.logName);
	else
		LogName.setU8(other.logName2);
	MiniDumpName.setU8(other.miniDumpName);
	}


