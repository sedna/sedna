#include <stdafx.h>
#include <string.h>
#include <stdio.h>

#if defined(_Windows) && !defined(_INC_WINDOWS)
#   include <windows.h>
#endif
#if !defined(DTSEARCH_H)
    #include <dtsearch.h>
#endif
#include <dt_util.h>

void dtsJobBase::reportError(int code, const char *msg, const char *arg1, const char *arg2)
{	if (!msg || !*msg)
		msg = dtsErrorInfo::getMessageForCode(code);

	if (errorHandler)
		errorHandler->handleMessage(code, msg, arg1, arg2);
	#if defined(DLOGGER_H) && defined(DSTRING_H)
		DString x;
		x << "ERROR #" << code << " " << msg;
		if (arg1 && *arg1)
			x << " " << arg1;
		if (arg2 && *arg2)
			x << " " << arg2;

		debugLog(x);
	#endif
}


//
//   These constructors and copy() functions can use memset and memove
//   because the structs are guaranteed not to have virtual functions
//   and because the pointers in them are supposed to be copied
//   as pointers (i.e., not by reallocating the string).
//


dtsFileSpec::dtsFileSpec()
{   clear();
}

void dtsFileSpec::clear()
{   memset(this, 0, sizeof(dtsFileSpec));
}

dtsFileSpec& dtsFileSpec::copy(const dtsFileSpec& other)
{   memmove(this, &other, sizeof(dtsFileSpec));
    return *this;
}

bool dtsFileDate::operator==(const dtsFileDate& other) const
{   return (day == other.day) && (month == other.month) && (year == other.year) &&
           (hour == other.hour) && (minute == other.minute) && (second == other.second);
}

bool dtsFileDate::operator!=(const dtsFileDate& other) const
{   return !(*this == other);
}

bool dtsFileDate::operator>(const dtsFileDate& other) const
{	if (year != other.year)
		return year > other.year;
	if (month != other.month)
		return month > other.month;
	if (day != other.day)
		return day > other.day;
	if (hour != other.hour)
		return hour > other.hour;
	if (minute != other.minute)
		return minute > other.minute;
	if (second != other.second)
		return second > other.second;
	return false;
}


dtsIndexJob::dtsIndexJob()
{	initStruct<dtsIndexJob>(*this);
}

void dtsIndexJob::clear()
{	clearStruct<dtsIndexJob>(*this);
}

dtsIndexJob& dtsIndexJob::copy(const dtsIndexJob& other)
{	copyStruct<dtsIndexJob>(*this, other);
    return *this;
}


dtsSearchJob::dtsSearchJob()
{	initStruct<dtsSearchJob>(*this);
}

void dtsSearchJob::clear()
{	clearStruct<dtsSearchJob>(*this);
}

dtsSearchJob& dtsSearchJob::copy(const dtsSearchJob& other)
{	copyStruct<dtsSearchJob>(*this, other);
    return *this;
};

dtsInitInfo::dtsInitInfo()
{   initStruct<dtsInitInfo>(*this);
}

dtsIndexInfo::dtsIndexInfo()
{	initStruct<dtsIndexInfo>(*this);
}

dtsOptions::dtsOptions()
{	initStruct<dtsOptions>(*this);

    // setup default values
    indexNumbers = true;
    hyphens = dtsoHyphenAsSpace;
    binaryFiles = dtsoIndexBinary;
    maxStoredFieldSize = 512;
    maxFieldNesting = 16;
    maxWordLength = 32;
    maxWordsToRetrieve = 8192*8;
    titleSize = 80;
    strcpy(stemmingRulesFile, "stemming.dat");
    strcpy(alphabetFile, "default.abc");
    strcpy(noiseWordFile, "noise.dat");
    strcpy(segmentationRulesFile, "fileseg.xml");
    strcpy(textFieldsFile, "fields.xml");
    strcpy(userThesaurusFile, "thesaur.xml");
    strcpy(fileTypeTableFile, "filetype.xml");
    strcpy(macroFile, "macros.xml");

    // Characters used for search features in search requests
    macroChar = '@';
    stemmingChar = '~';
    phonicChar = '#';
    weightChar = ':';
    synonymChar = '&';
    fuzzyChar = '%';
    matchDigitChar = '=';

    // Unicode filtering options
    unicodeFilterBlockSize = 256 * 1024;
    unicodeFilterRanges[0] = 1;
    unicodeFilterRanges[1] = 1;
    unicodeFilterRanges[2] = 1;
    unicodeFilterMinTextSize = 6;

}

static void LimitValue(long& val, long lowBound, long highBound)
{   if (val < lowBound)
        val = lowBound;
    if (val > highBound)
        val = highBound;
}

//
// Enforce boundaries on option values
void dtsOptions::validate()
{   LimitValue(maxWordsToRetrieve, 16, 32767 * 8);
    LimitValue(maxStoredFieldSize, 4, 8192);
    LimitValue(maxFieldNesting, 1, 32);
    LimitValue(titleSize, 4, 2048);
    LimitValue(maxWordLength, 4, 128);
    LimitValue(unicodeFilterBlockSize, 1024, 1024 * 1024 * 20);
    if (unicodeFilterMinTextSize)
		LimitValue(unicodeFilterMinTextSize, 3, 1024);
}

void dtsOptions::copy(const dtsOptions& other)
{	copyStruct<dtsOptions>(*this, other, sizeof(dtsOptions));
}

bool dtsOptions::equals(const dtsOptions& other) const
{	if (other.structSize != structSize)
		return false;
	if (!memcmp(this, &other, structSize))
		return true;

	if (binaryFiles != other.binaryFiles)
		return false;
	if (strcmp(binaryFilterTextChars, other.binaryFilterTextChars))
		return false;
	if (hyphens != other.hyphens)
		return false;
	if (strcmp(alphabetFile, other.alphabetFile))
		return false;
	if (indexNumbers != other.indexNumbers)
		return false;
	if (strcmp(noiseWordFile, other.noiseWordFile))
		return false;
	if (strcmp(stemmingRulesFile, other.stemmingRulesFile))
		return false;
	if (maxWordsToRetrieve != other.maxWordsToRetrieve)
		return false;
	if (maxStoredFieldSize != other.maxStoredFieldSize)
		return false;
	if (titleSize != other.titleSize)
		return false;
	if (strcmp(xmlIgnoreTags, other.xmlIgnoreTags))
		return false;
	if (maxWordLength != other.maxWordLength)
		return false;
	if (strcmp(segmentationRulesFile, other.segmentationRulesFile))
		return false;
	if (strcmp(textFieldsFile, other.textFieldsFile))
		return false;
	if (strcmp(userThesaurusFile, other.userThesaurusFile))
		return false;
	if (strcmp(homeDir, other.homeDir))
		return false;
	if (strcmp(privateDir, other.privateDir))
		return false;
	if (strcmp(booleanConnectors, other.booleanConnectors))
		return false;
	if (strcmp(fileTypeTableFile, other.fileTypeTableFile))
		return false;
	if (textFlags != other.textFlags)
		return false;
	if (maxFieldNesting != other.maxFieldNesting)
		return false;
	if (strcmp(macroFile, other.macroFile))
		return false;
	if (macroChar != other.macroChar)
		return false;
	if (fuzzyChar != other.fuzzyChar)
		return false;
	if (phonicChar != other.phonicChar)
		return false;
	if (stemmingChar != other.stemmingChar)
		return false;
	if (synonymChar != other.synonymChar)
		return false;
	if (weightChar != other.weightChar)
		return false;
	if (matchDigitChar != other.matchDigitChar)
		return false;
	if (fieldFlags != other.fieldFlags)
		return false;
	if (memcmp(unicodeFilterRanges, other.unicodeFilterRanges, sizeof other.unicodeFilterRanges))
		return false;
	if (unicodeFilterBlockSize != other.unicodeFilterBlockSize)
		return false;
	if (unicodeFilterFlags != other.unicodeFilterFlags)
		return false;
	if (unicodeFilterMinTextSize != other.unicodeFilterMinTextSize)
		return false;
	if (unicodeFilterWordOverlapAmount != other.unicodeFilterWordOverlapAmount)
		return false;
	return true;
}

dtsRetrievedFileTable::dtsRetrievedFileTable(long aMaxCount) :
    fileTable(0),
    sortOrder(0),
    maxCount(0),
    fileCount(0)
{   allocTable(aMaxCount);
}

void dtsRetrievedFileTable::copy(const dtsRetrievedFileTable& other)
{   fileCount = other.fileCount;
    allocTable(other.maxCount);
    memmove(fileTable, other.fileTable, sizeof(dtsRetrievedFile) * other.maxCount);
    memmove(sortOrder, other.sortOrder, (sizeof sortOrder[0]) * other.maxCount);
}

void dtsRetrievedFileTable::allocTable(long aMaxCount)
{   if (fileTable) {
        delete [] fileTable;
        fileTable = 0;
        }
    if (sortOrder) {
        delete [] sortOrder;
        sortOrder = 0;
        }
    if (aMaxCount) {
        fileTable = new dtsRetrievedFile[aMaxCount];
        sortOrder = new long[aMaxCount];
        }
    maxCount = aMaxCount;
}

dtsRetrievedFileTable::~dtsRetrievedFileTable()
{   allocTable(0);
}

void dtsRetrievedFileTable::clear()
{   fileCount = 0;
}

dtsRetrievedFile *dtsRetrievedFileTable::getInOrder(int i)
{   if ((i >= 0) && (i < fileCount)) {
        long j = sortOrder[i];
        return &fileTable[j];
        }
    return 0;
}

dtsFileConvertJob::dtsFileConvertJob()
{   initStruct<dtsFileConvertJob>(*this);
}

dtsFileConvertJob2::dtsFileConvertJob2()
{	initStruct<dtsFileConvertJob2>(*this);
}

dtsIndexMergeJob::dtsIndexMergeJob()
{	initStruct<dtsIndexMergeJob>(*this);
}

dtsMergeStatusInfo::dtsMergeStatusInfo()
{	initStruct<dtsMergeStatusInfo>(*this);
}

dtsIndexVerifyJob::dtsIndexVerifyJob()
{	initStruct<dtsIndexVerifyJob>(*this);
}

dtsVerifyStatusInfo::dtsVerifyStatusInfo()
{	initStruct<dtsVerifyStatusInfo>(*this);
}

dtsIndexProgressInfo::dtsIndexProgressInfo()
{   memset(this, 0, sizeof(dtsIndexProgressInfo));
}

dtsMapHitsJob::dtsMapHitsJob(dtsSearchResultsItem& aItem)
{	initStruct<dtsMapHitsJob>(*this);

    filename = aItem.filename;

    table = new dtsHitLocation[aItem.hitCount];
    nTableEntries = aItem.hitCount;

    for (long i = 0; i < aItem.hitCount; ++i)
        table[i].wordOffset = aItem.hits[i];
}

dtsMapHitsJob::dtsMapHitsJob(long tableSize)
{	initStruct<dtsMapHitsJob>(*this);

    table = new dtsHitLocation[tableSize];
    nTableEntries = tableSize;
    filename = 0;
}

dtsMapHitsJob::~dtsMapHitsJob()
{   delete[] table;
    table = 0;
    nTableEntries = 0;
}

dtsErrorInfo::dtsErrorInfo(int aMaxMessages, int aBufSize) :
    structSize(sizeof (dtsErrorInfo) ),
    pUserData(0),
    messageCode(new long[aMaxMessages]),
    buf(new char[aBufSize]),
    messageText(new const char *[aMaxMessages]),
    messageArg1(new const char *[aMaxMessages]),
    messageArg2(new const char *[aMaxMessages]),
    bufSize(aBufSize),
    maxMessages(aMaxMessages),
    pNotifyFn(0),
    messageCount(0),
    textSize(0),
    fOverflowed(0)
{   memset(&reserved, 0, sizeof reserved);
}

void dtsErrorInfo::clear()
{   messageCount = 0;
    textSize = 0;
    fOverflowed = 0;
}

dtsErrorInfo::~dtsErrorInfo()
{   delete[] messageCode;
    delete[] buf;
    delete [] messageText;
    delete [] messageArg1;
    delete [] messageArg2;
}

int dtsErrorInfo::overflowed() const
{   return (fOverflowed ? 1 : 0);
}

void dtsErrorInfo::setNotify(int (*aNotifyFn)(dtsErrorInfo *pHandler))
{   pNotifyFn = aNotifyFn;
}

long dtsErrorInfo::getCount() const
{   return messageCount;
}

int dtsErrorInfo::handleMessage(long code, const char *msg, const char *arg1,
            const char *arg2)
{   if (messageCount >= maxMessages) {
		fOverflowed = true;
		if (pNotifyFn)
			// Since there is a notify function, it is useful to send all of the
			// messages.  Otherwise, just stop storing messages when the buffer fills
			clear();
		else
			return -1;
        }
    // if msg is not formated in correct message format, add the necessary
    // codes.  Messages are supposed to start with NNNN $X where
    // X = message type (Q = question, I = info, E = error, D = Diagnostic)
    // NNNN = message code

    if (msg[0] != '$') {
		int msgsize = strlen(msg) + 64;
        char *tmp = new char[msgsize+1];
        _snprintf(tmp, msgsize, "$E %4.4ld %s", code, msg);
        messageText[messageCount] = storeString(tmp);
        delete tmp;
        }
    else
        messageText[messageCount] = storeString(msg);

    messageArg1[messageCount] = storeString(arg1);
    messageArg2[messageCount] = storeString(arg2);
    messageCode[messageCount] = code;
    messageCount++;
    if (pNotifyFn)
        return pNotifyFn(this);
    else
        return -1;
}

const char *dtsErrorInfo::storeString(const char *s)
{   if (!s)
        s = "";
    int l = (int) strlen(s) + 1;
    const char *ret = "";
    if (textSize + l < bufSize) {
        strcpy(buf + textSize, s);
        ret = buf + textSize;
        textSize += l;
        }
    else
        fOverflowed = true;
    return ret;
}

const char *dtsErrorInfo::getMessage(long iMessage) const
{   // Default is to get the last message
    if (iMessage == -1)
        iMessage = messageCount-1;

    if ((iMessage < messageCount) && (iMessage >= 0))
        return messageText[iMessage];
    else
        return "";
}

long dtsErrorInfo::getMessageCode(long iMessage) const
{   // Default is to get the last message
    if (iMessage == -1)
        iMessage = messageCount-1;
    if ((iMessage < messageCount) && (iMessage >= 0))
        return messageCode[iMessage];
    else
        return 0;
}

int dtsErrorInfo::haveMessage(long code) const
{   for (int i = 0; i < messageCount; ++i) {
        if (messageCode[i] == code)
            return true;
        }
    return false;
}

const char *dtsErrorInfo::getMessageArg1(long iMessage) const
{   // Default is to get the last message
    if (iMessage == -1)
        iMessage = messageCount-1;
    if ((iMessage < messageCount) && (iMessage >= 0))
        return messageArg1[iMessage];
    else
        return "";
}

const char *dtsErrorInfo::getMessageArg2(long iMessage) const
{   // Default is to get the last message
    if (iMessage == -1)
        iMessage = messageCount-1;
    if ((iMessage < messageCount) && (iMessage >= 0))
        return messageArg2[iMessage];
    else
        return "";
}

void dtsErrorInfo::forwardTo(dtsErrorInfo& other)
{   for (int i = 0; i < getCount(); ++i)
        other.handleMessage(getMessageCode(i), getMessage(i), getMessageArg1(i), getMessageArg2(i));
}

void dtsErrorInfo::copy(const dtsErrorInfo& other)
{   clear();
    if (other.textSize >= bufSize) {
        delete [] buf;
        bufSize = other.textSize + 1;
        buf = new char[bufSize];
        }
    if (other.messageCount > maxMessages) {
        delete [] messageCode;
        delete [] messageText;
        delete [] messageArg1;
        delete [] messageArg2;
        maxMessages = other.messageCount;
        messageCode = new long[maxMessages];
        messageText = new const char *[maxMessages];
        messageArg1 = new const char *[maxMessages];
        messageArg2 = new const char *[maxMessages];
        }
    pNotifyFn = 0;
    for (int i = 0; i < other.messageCount; ++i) {
        handleMessage(other.getMessageCode(i),
            other.getMessage(i),
            other.getMessageArg1(i),
            other.getMessageArg2(i));
        }
    pNotifyFn = other.pNotifyFn;
    structSize = sizeof(dtsErrorInfo);
    fOverflowed = other.fOverflowed;
}

// dtsSearchResultsItem is used to obtain information about search results from the
// dtSearch Engine.  If the dtSearch Engine DLL is a different version, structures
// returned from the engine may be larger or smaller than the calling program
// expects.  Therefore (1) check the structSize on copy() and clear() operations, and
// (2) preserve the structSize in the structure.
dtsSearchResultsItem::dtsSearchResultsItem()
{	initStruct<dtsSearchResultsItem>(*this);
}

void dtsSearchResultsItem::copy(const dtsSearchResultsItem& other)
{	copyStruct<dtsSearchResultsItem>(*this, other, sizeof(dtsSearchResultsItem));
}


void dtsSearchResultsItem::clear()
{	clearStruct<dtsSearchResultsItem>(*this);
}

//  A file map is a table mapping word counts to binary offsets and page/paragraph
//  offsets in a document.  This makes it possible to find hits in long documents
//  more quickly.
void dtsSearchResultsItem::getFileMapEntryForWord(long w, dtsFileMapEntry& dest)
{   dest.clear();

    if (!fileMapEntryCount || !fileMapTable)
        return;

    // If the first entry in the file map table is past the word,
    // return the blank dtsFileMapEntry, which indicates the start of the file.
    if (fileMapTable[0].wordCount > w)
        return;

    for (int p = 0; p < fileMapEntryCount; ++p) {
        dtsFileMapEntry& fme = fileMapTable[p];
        if (fme.wordCount <= w) {
            if ((p == fileMapEntryCount-1) ||
                (w <= fileMapTable[p+1].wordCount)) {

                dest = fme;
                return;
                }
            }
        }
}

static int isBlank(const char *p)
{   if (!p || !*p)
        return true;
    if ((*p > 32) || (*p < 0))
        return false;

    while(*p) {
        if (!isspace(*p))
            return false;
        p++;
        }
    return true;
}

static const char *findTail(const char *fn)
{   if (!fn || !*fn)
        return 0;
    size_t last = strlen(fn)-1;
    if (last == 0)
		return fn;

    for (int i = (int) last; i >= 0; --i) {
        if  ((fn[i] == '\\') || (fn[i] == '/'))
            return fn + i + 1;
        }
    return fn;
}

const char *dtsSearchResultsItem::getDisplayName(int fPdfUseTitleAsName, int fHtmlUseTitleAsName) const
{   const char *ret = findTail(filename);
	if (!ret)
		return filename;
    // check for container filename
    if (strchr(ret, '>') && strchr(ret, '|')) {
        ret = strrchr(ret, '|');
        ret++;
        }
    bool bHaveDisplayName = (!isBlank(displayName) && stricmp(displayName, filename));
    if (typeId == it_PDF) {
        if (fPdfUseTitleAsName && bHaveDisplayName)
            ret = displayName;
        }
    else if (typeId == it_HTML) {
        if (fHtmlUseTitleAsName && bHaveDisplayName)
            ret = displayName;
        }
    else if (bHaveDisplayName)
        ret = displayName;
    return ret;
}

const char *dtsSearchResultsItem::getTitle(int fPdfUseTitleAsName, int fHtmlUseTitleAsName) const
{   const char *ret = title;
    if ((typeId == it_PDF) && (fPdfUseTitleAsName == 0) && !isBlank(displayName))
        ret = displayName;
    if ((typeId == it_HTML) && (fHtmlUseTitleAsName == 0) && !isBlank(displayName))
        ret = displayName;
    return ret;
}

// Get a string that describes the type name
// Note: The same type name may have more than one parser (for example,
// Microsoft Word), so multiple typeId values may map to a single string.
const char *dtsSearchResultsItem::getTypeName()
{   int id = typeId;
    if ((id == it_RTF) && strchr(filename, '|'))
        id = it_DatabaseRecord;
	return getTypeName(id);
}

const char *dtsSearchResultsItem::getTypeName(int typeId) {
switch(typeId) {
case it_Ami              : return "Ami";
case it_Ansi             : return "Ansi";
case it_Ascii            : return "Ascii";
case it_Binary           : return "Binary";
case it_CompoundDoc      : return "Office";
case it_DBF              : return "XBase";
case it_FilteredBinary   : return "Binary (filtered)";
case it_HyperText        : return "Hypertext";
case it_MS_Word          : return "Microsoft Word";
case it_MS_Works         : return "Microsoft Workstype";
case it_Multimate        : return "Multimate";
case it_RTF              : return "RTF";
case it_WS_2000          : return "WordStar 2000";
case it_WS_5             : return "WordStar";
case it_WinWrite         : return "Write";
case it_WordForWin       : return "Microsoft Word";
case it_WordForWin6      : return "Microsoft Word";
case it_WordForWin97	 : return "Microsoft Word";
case it_MicrosoftWord	 : return "Microsoft Word";
case it_DocFile          : return "Microsoft DocFile";
case it_WordPerfect42    : return "WordPerfect 4.2";
case it_WordPerfect5     : return "WordPerfect 5";
case it_WordPerfect6     : return "WordPerfect";
case it_WordStar         : return "WordStar";
case it_XyWrite          : return "XyWrite";
case it_ZIP              : return "ZIP";
case it_Properties       : return "Document Summary";
case it_Excel5           : return "Microsoft Excel";
case it_HTML             : return "HTML";
case it_PDF              : return "PDF";
case it_Excel97          : return "Microsoft Excel";
case it_PowerPoint       : return "Microsoft PowerPoint";
case it_EncryptedHtml    : return "HTML*";
case it_DatabaseRecord   : return "Database Record";
case it_SegmentedText    : return "Segmented Text";
case it_XML              : return "XML";
case it_WordPerfectEmbedded : return "WordPerfect";
case it_Unicode         : return "Unicode";
case it_EudoraMessage    : return "Eudora Message";
case it_OutlookExpressMessage : return "Outlook Express Message";
case it_Utf8                : return "UTF-8";
case it_DjVu             :  return "DjVu";
case it_SingleByteText   :  return "Text";
case it_MimeMessage      :  return "Email in MIME file";
case it_EML 			: return "MIME file";
case it_MBoxArchive    : return "Email archive";
case it_ZIP_zlib	   : return "ZIP";
case it_OutlookMsgFile : return "Outlook MSG";
case it_TreepadHjtFile : return "TreePad HJT";
case it_PfsProfessionalWrite : return "PFS Write";
case it_FilteredBinaryUnicodeStream : return "Filtered binary";
case it_Media :			return "Music/Video";
case it_NonTextData :	return "Data";
case it_DatabaseRecord2: return "Database Record";
case it_Excel2003Xml:	return "Microsoft Excel 2003 XML";
case it_Word2003Xml:    return "Microsoft Word 2003 XML";
case it_OpenOfficeDocument:  return "OpenOffice Document";

default: return " ";
}
}


dtsFileMapEntry::dtsFileMapEntry()
{   memset(this, 0, sizeof(dtsFileMapEntry));
}

void dtsFileMapEntry::clear()
{   memset(this, 0, sizeof(dtsFileMapEntry));
}

int dtsFileMapEntry::operator==(const dtsFileMapEntry& other) const
{   return (wordCount == other.wordCount);
}

dtsFileMapEntry::dtsFileMapEntry(const dtsFileMapEntry& other)
{   *this = other;
}

dtsFileMapEntry& dtsFileMapEntry::operator=(const dtsFileMapEntry& other)
{   memmove(this, &other, sizeof(dtsFileMapEntry));
    return *this;
}

dtsOutputInfo::dtsOutputInfo()
{   initStruct<dtsOutputInfo>(*this);
}

void dtsOutputInfo::clear()
{	clearStruct<dtsOutputInfo>(*this);
}

void dtsOutputInfo::copy(const dtsOutputInfo& other)
{	copyStruct<dtsOutputInfo>(*this, other);
}

dtsSearchReportJob::dtsSearchReportJob()
{	initStruct<dtsSearchReportJob>(*this);
}

void dtsSearchReportJob::copy(const dtsSearchReportJob& other)
{	copyStruct<dtsSearchReportJob>(*this, other);
}

dtsOnIndexWordInfo::dtsOnIndexWordInfo()
{	initStruct<dtsOnIndexWordInfo>(*this);
}

dtsCrashHandlerInfo::dtsCrashHandlerInfo()
{	initStruct<dtsCrashHandlerInfo>(*this);
}

void dtsCrashHandlerInfo::copy(const dtsCrashHandlerInfo& other)
{	copyStruct<dtsCrashHandlerInfo>(*this, other);
}

dtsListIndexJob::dtsListIndexJob()
{	initStruct<dtsListIndexJob>(*this);
}

void dtsIndexInfo::clear()
{	memset(this, 0, sizeof(dtsIndexInfo));
}


const char *dtsErrorInfo::getMessageForCode(int	code) {
	switch(code) {

		case dtsErAccIndex:
			return "Unable to access index  ";
		case dtsAskDiskFullOverride:
			return "There may not be enough space to update index.  Continue anyway?";
		case dtsErOpenTemp:
			return "Unable to open temporary file.  ";
		case dtsErIxWrongVersion:
			return "Index built by incompatible version of dtSearch";
		case dtsErIxInterrupted:
			return "Index corrupt because indexing was interrupted.";
		case dtsErAccDirectory:
			return "Unable to create or access directory";
		case dtsErAccFile:
			return "Unable to access input file";
		case dtsErIndexFull:
			return "Index is full";
		case dtsErPdfLzwNotLicensed:
			return "Attempt to index a PDF file with LZW and LZW decoding is not licensed";
		case dtsErTimeout:
			return "Operation timed out";
		case dtsErCreateFailed:
			return "Unable to create index";
		case dtsErCommitFailed:
			return "Unable to commit changes to index";
		case dtsErLowMemory:
			return "Search halted due to low memory available on system";
		case dtsErBadRequest:
			return "Syntax error in search request";
		case dtsErSearchLimitReached :
			return "Search found more files than the limit for the search job";
		case dtsErNoFilesRetrieved:
			return "No files retrieved in search";
		case dtsErRequestTooLong:
			return "Search request was too long";
		case dtsErMaxWords:
			return "Too many words retrieved in index";
		case dtsErDiskFull:
			return "Disk full -- indexing halted";
		case dtsErIxCorrupt:
			return "Index is corrupt";
		case dtsErAccessDenied:
			return "Access to the index was denied";
		case dtsErFileNotFound:
			return "The file was not found in the index";
		case dtsErFileEncrypted:
			return "The file is encrypted";
		case dtsErFileCorrupt:
			return "The file is corrupt";
		case dtsErAccCachedDoc:
			return "Unable to to access cached document in index";
		case dtsErIndexingError:
			return "Index update could not complete due to an error accessing the index";
		case dtsErOutOfMemory:
			return "Out of memory";
		case dtsErUnknownException:
			return "Unknown Exception";
		default:
			return "Unknown	error code";
		}
	}

