#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <string>
#include <list>
#include <hash_map>
#include <hash_set>
#include <stdexcept>
#include <limits>
#include <algorithm>
#include <cstdio>
#include <windows.h>

#ifdef _MSC_VER
#define snprintf _snprintf
#endif

using namespace std;
using namespace stdext;

#define SPACE " \n\t"
#define SPACE_CHARS " \n\t"

class GeneratorState
{	
	struct ErrorEntry
	{
		string name;
		string description;
		int code;
	};

	friend bool spred(const ErrorEntry &, const ErrorEntry &);

	struct SectionHeader
	{
		string description;
		int position;
	};

	hash_map<string,int> legacyCodes;
	hash_set<int> assignedCodes;
	list<ErrorEntry> errorEntries;
	list<SectionHeader> sectionHeaders;
	int firstUnusedCode;

public:
	void Reset()
	{
		firstUnusedCode=0;
		legacyCodes.clear();
		assignedCodes.clear();
		errorEntries.clear();
		sectionHeaders.clear();
	}
	GeneratorState() { Reset(); }
	void WriteLegacyCodesFile(ostream &os) const;
	void WriteHeaderFile(ostream &os, const string &prefix) const;
	void ReadLeagacyCodesFile(istream &is);
	void AssignCodes();
	void ReadDataFile(istream &is);
	void WriteStringTable(ostream &os) const;
};

string Int2Str(int v)
{
	char str[64]; 
	return string(str,snprintf(str,64,"%d",v));
}

void GeneratorState::WriteLegacyCodesFile(ostream &os) const 
{
	size_t codeW=0;
	codeW=Int2Str(firstUnusedCode-1).size();

	for (list<ErrorEntry>::const_iterator i=errorEntries.begin();
		 i!=errorEntries.end();
		 ++i)
	{
		os<<setw(codeW)<<i->code<<" "<<i->name<<endl;
	}
}

void GeneratorState::WriteHeaderFile(ostream &os, const string &prefix) const
{
	string upcasePrefix(prefix.size(),' '), guard, firstErr;
	int currentPosition=0, emptyLineNow=1;
	size_t nameW=0, codeW=0;
	list<SectionHeader>::const_iterator hi=sectionHeaders.begin();

	transform(prefix.begin(),prefix.end(),upcasePrefix.begin(),toupper);
	guard=upcasePrefix+"_INCLUDED";
	firstErr=upcasePrefix+"_FIRST_ERR";
	codeW=Int2Str(firstUnusedCode-1).size();

	for (list<ErrorEntry>::const_iterator i=errorEntries.begin();
		 i!=errorEntries.end();
		 ++i)
	{
		nameW=max(nameW,i->name.size());
	}

	os << "/* GENERATED FILE, DO NOT EDIT! */ " << endl;
	os << "#if (_MSC_VER > 1000)" << endl; 
	os << "#pragma once" << endl;
	os << "#endif" << endl;
	os << "#ifndef " << guard << endl;
	os << "#define " << guard << endl;
	os << endl;
	os << "#ifndef " << firstErr << endl;
	os << "#define " << firstErr << " " << 0 << endl;
	os << "#endif" << endl;

	for (list<ErrorEntry>::const_iterator i=errorEntries.begin();
		 i!=errorEntries.end();
		 ++i, ++currentPosition)
	{
		while (hi!=sectionHeaders.end() && hi->position==currentPosition)
		{
			os << endl;
			os << "/* " << hi->description << " */ " << endl;
			emptyLineNow=1;
			++hi;
		}
		if (emptyLineNow)
		{
			emptyLineNow=0;
			os << endl;
		}
		os 
			<< "#define " << left << setw(nameW) << i->name
			<< " (" << right << setw(codeW) << i->code << " + " << firstErr << ")" << endl;
	}

	while (hi!=sectionHeaders.end())
	{	
		os << endl;
		os << "/* " << hi->description << " */ " << endl;
		++hi;
	}

	os << endl;

	os << "#endif" << endl;
}

void GeneratorState::ReadLeagacyCodesFile(istream &is)
{
	string buf, name;
	istringstream is2;
	int code, lastUsedCode=firstUnusedCode-1;

	is2.exceptions(ios::failbit);
	while (getline(is,buf,'\n').good())
	{
		is2.clear();
		is2.str(buf);
		is2 >> code >> name;
		if (legacyCodes.count(name)>0)
		{
			if (legacyCodes[name]!=code) throw runtime_error("duplicate entry "+name);
		}
		if (assignedCodes.count(code)>0)
		{
			throw runtime_error("duplicate code " + Int2Str(code));
		}
		legacyCodes.insert(pair<string,int>(name,code));
		assignedCodes.insert(code);
		lastUsedCode=max(lastUsedCode,code);
	}
	firstUnusedCode=max(firstUnusedCode,lastUsedCode+1);
}

void GeneratorState::AssignCodes()
{
	hash_map<string,int>::iterator r;
	hash_set<string> knownNames;
	for (list<ErrorEntry>::iterator i=errorEntries.begin(); 
		 i!=errorEntries.end();
		 ++i)
	{
		if (knownNames.count(i->name)>0)
		{
			throw runtime_error("duplicate entry " + i->name);
		}
		knownNames.insert(i->name);
		r = legacyCodes.find(i->name);
		if (r!=legacyCodes.end())
		{
			i->code = r->second;
		}
		else
		{
			i->code = firstUnusedCode++;
		}
	}
}

void GeneratorState::ReadDataFile(istream &is)
{
	string buf;
	istringstream is2;
	int line=1;
	size_t ofs, ofs2;
	ErrorEntry errorEntry;
	SectionHeader sectionHeader;

	is2.exceptions(ios::failbit);
	try
	{
		while (getline(is,buf,'\n').good())
		{
			is2.clear();
			ofs = buf.find_first_not_of(SPACE_CHARS);

			if (ofs==-1)
			{
				/* empty line, ignore */ 
			}
			else if (buf[ofs]=='#')
			{
				if (ofs+1<buf.size() && buf[ofs+1]=='#')
				{
					/* section header */ 
					ofs=buf.find_first_not_of(SPACE_CHARS,ofs+2);
					ofs2=buf.find_last_not_of(SPACE_CHARS)+1;
					if (ofs!=-1)
					{
						sectionHeader.description=buf.substr(ofs,ofs2-ofs);
						sectionHeader.position=errorEntries.size();
						sectionHeaders.push_back(sectionHeader);
					}
				}
				else
				{
					/* comment, ignore */ 
				}
			}
			else
			{
				/* error entry */ 
				if (ofs!=0) throw 0;
				is2.str(buf);
				is2>>errorEntry.name;
				errorEntry.code=0;
				++line;
				if (getline(is,buf,'\n').bad()) throw 0;
				ofs=buf.find_first_not_of(SPACE_CHARS);
				ofs2=buf.find_last_not_of(SPACE_CHARS)+1;
				if (ofs!=1 || ofs2-ofs==0 || buf[0]!='\t') throw 0;
				errorEntry.description=buf.substr(ofs,ofs2-ofs);
				errorEntries.push_back(errorEntry);
			}
			++line;
		}
	}
	catch (...)
	{
		throw runtime_error("syntax error at line "+Int2Str(line));
	}
}

inline 
bool spred(const GeneratorState::ErrorEntry &first, const GeneratorState::ErrorEntry &second)
{
	return first.code<second.code;
}

void GeneratorState::WriteStringTable(ostream &os) const
{
	int n=0, e=0, nl=0;
	string header("my-errors.h");

	list<ErrorEntry> errorEntries(this->errorEntries);
	errorEntries.sort(spred);
	os << "#include <stddef.h>" << endl;
	os << "#include \"" << header << "\"" << endl;
	os << endl;
	os << "static const size_t stringTableSize = "<<errorEntries.size()<<";"<<endl;
	os << "static const char *stringTable[stringTableSize] = " << endl;
	os << "{" << endl;
	for (list<ErrorEntry>::iterator i=errorEntries.begin(); 
		 i!=errorEntries.end();
		 ++i, ++n)
	{
		if (i->code>n) 
		{ 
			e=0;
			nl=1;
			for (;i->code>n;++n)
			{
				if (nl) 
				{
					os<<"\t";
					nl=0;
				}
				os << "NULL, ";
				if (++e%8==0) 
				{
					nl=1;
					os<<endl;
				}
			}
			if (!nl) os<<endl;
		}
						
		os << "\t\"" << i->description << "\"" << (n<errorEntries.size()-1?", ":"") << endl;
	}
	os << "};" << endl;
	os << endl;
	os << "const char * strerror(int error)" << endl;
	os << "{" << endl;
	os << "	error-=WUERR_FIRST_ERR;" << endl;
	os << "	return (error>=0 && error<stringTableSize ? stringTable[error] : NULL);" << endl;
	os << "}" << endl;

}

int main()
{
	GeneratorState generatorState;
	ifstream pinf;
	//DebugBreak();
	try
	{
		pinf.open("pin");
		if (pinf.is_open()) generatorState.ReadLeagacyCodesFile(pinf);
		generatorState.ReadDataFile(cin);
		generatorState.AssignCodes();
		generatorState.WriteLegacyCodesFile(cerr);
		generatorState.WriteHeaderFile(cout,"wuerr");
		generatorState.WriteStringTable(cout);
	}
	catch (exception &e)
	{
		cerr << e.what() << endl;
	}
	catch (...)
	{
		cerr << "unrecognised exception" << endl;
	}
	return 0;
}
