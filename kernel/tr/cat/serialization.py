
import sys
import re

class Variable :
    pass

re_ident = re.compile('(enum|struct)?\ ?[\<\>A-Za-z0-9\_]+');
vlist = []

for line in sys.stdin.xreadlines():
    i = line.find('persistent');
    if (i > -1) :
        pre = (line[(i + 10):]).lstrip()
        v = Variable()
        if (pre.startswith('string')) : v.mspec = 1
        elif (pre.startswith('special')) : v.mspec = 2
        else : v.mspec = 0
        j = line.find('//')
        if j > -1 :
            line = line[:j]
        line = line.strip()
        
        while 1 :
            j = line.find('/*')
            if (j == -1) : break
            j1 = line.find('*/', j)
            if (j == -1) : break
            line = line[:j] + line[(j1 + 2):]
        
        v.mline = line
        m = re_ident.match(line)
        avartype = m.group()
        line = line[m.end():].lstrip();
        while 1 :
            if (line[0] == '*') :
                ast = '*'
                line = line[1:].lstrip();
            else : ast = ''
            
            m = re_ident.match(line)
            line = line[m.end():].lstrip();
            
            v.mvartype = avartype + ast;
            v.mvarname = m.group()
            
            if (line[0] == ',') :
                line = line[1:].lstrip()
            elif (line[0] == ';') : break
            else : break
            
        vlist.append(v);
#    line.rfind('//');
    
for v in vlist :
    if (v.mspec == 0) : 
        print '    stream.write(&%s, sizeof(%s));' % (v.mvarname, v.mvartype);
    elif (v.mspec == 1) :
        print '    stream.write_string(%(0)s);' % { '0' : v.mvarname }
    elif (v.mspec == 2) :
        print '    /* %s serialization (%s) */' % (v.mvarname, v.mvartype);
    
print ' '

for v in vlist :
    if (v.mspec == 0) : 
        print '    stream.read(&%s, sizeof(%s));' % (v.mvarname, v.mvartype);
    elif (v.mspec == 1) :
        print '    %(0)s = (char *) cat_malloc_context(CATALOG_COMMON_CONTEXT, stream.read_string_len());\n    stream.read_string(SSTREAM_SAVED_LENGTH, %(0)s);' % { '0' : v.mvarname };
    elif (v.mspec == 2) :
        print '    /* %s deserialization (%s) */' % (v.mvarname, v.mvartype);
    
