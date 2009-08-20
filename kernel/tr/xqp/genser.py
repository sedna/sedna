import os

files = os.listdir('./ast/')
files.sort()

for file in files:
    if not file.endswith('.h'):
        continue

    name = file[0:-2]

    if name == 'AST' or name == 'ASTNode':
        continue

    fn = open(os.path.join('ast/', file), 'rb')

    fh = open(os.path.join('ast/', file + '.bak'), 'wb')
    fc = open(os.path.join('ast/', name, '.c'), 'wb')

    var_mode = False

    for l in fn.readlines():
        if l.startswith('public:') and not var_mode:
            var_mode = True
        elif l.startswith('public:'):
            var_mode = False
        elif l.startswith('};'):
            print >>fh, '    static ', name, '* fromSList(scheme_list *slist);'
            print >>fh, '};'
            print >>fh
            break

        print >>fh, l

        if var_mode:
            l = l[:-1] # strip \n

            type_name = l.split()[0]
            vars = l.split()[1:]

            if type_name == '//' or not type_name.startswith('AST') or type_name != 'std::string':
                continue

            for var in vars:
                if not var.startswith('*'):
                    continue

                var = var.strip()[1:]

                if var.endswith(',') or var.endswith(';'):
                    var = var[:-1]

                if type_name == 'ASTNodesVector':
                    print >>fc, '    VisitNodesVector(', var, ', *this);'
                else:
                    print >>fc, '    n.', var, '->accept(*this);'

    print >>fc, '    lr_str.append(")");\n}\n'

    fn.close()

print >>fh, '};'

fh.close()
fc.close()
