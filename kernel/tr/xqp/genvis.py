#! /usr/bin/python

import os

astfiles = os.listdir('./ast/')
astfiles.sort()

for filea in astfiles:
    if not filea.endswith('.h'):
        continue

    class_name = filea[0:-2]

    if class_name == 'AST' or class_name == 'ASTNode':
        continue

    fh = open(os.path.join('ast/', filea), 'r')

    lines_h = []
    lines_cpp = []

    var_mode = False
    class_vars = []

    for l in fh:
        lines_h.append(l)

        if l.startswith('//'):
            continue

        l = l.strip()
        if l.startswith('public:'):
            var_mode = not var_mode
            continue

        if var_mode:
            if len(l.split()) == 0:
                continue

            type_name = l.split()[0]

            if type_name != 'ASTNode' and type_name != 'ASTNodesVector':
                continue

            vars_o = l.split()[1:]
            vars_n = []

            for var in vars_o:
                var = var.strip()

                if var == '//':
                    break

                if var.startswith('*'):
                    var = var[1:]

                if var.endswith(',') or var.endswith(';'):
                    var = var[:-1]

                vars_n.append(var)

            class_vars.append((vars_n, type_name))
        else:
            if (l == 'ASTNode *dup();'):
                lines_h.append('    void modifyChild(const ASTNode *oldc, ASTNode *newc);\n')

    fh.close()

    fh = open(os.path.join('ast/', filea), 'w')
    for l in lines_h:
        print >>fh, l,

    fh.close()

    fc = open(os.path.join('ast/', class_name + '.cpp'), 'a')

    print >>fc, '\nvoid ' + class_name + '::modifyChild(const ASTNode *oldc, ASTNode *newc)'
    print >>fc, '{'

    for varp in class_vars:
        vari = varp[0]
        vart = varp[1]

        for i in vari:
            if vart == 'ASTNode':
                print >>fc, '    if (' + i + ' == oldc)'
                print >>fc, '    {'
                print >>fc, '        ' + i + ' = newc;'
                print >>fc, '        return;'
                print >>fc, '    }'
            else:
                print >>fc, '    if (' + i + ')'
                print >>fc, '    {'
                print >>fc, '        for (unsigned int i = 0; i < ' + i + '->size(); i++)'
                print >>fc, '        {'
                print >>fc, '            if ((*' + i + ')[i] == oldc)'
                print >>fc, '            {'
                print >>fc, '                (*' + i + ')[i] = newc;'
                print >>fc, '                return;'
                print >>fc, '            }'
                print >>fc, '        }'
                print >>fc, '    }'

    print >>fc, '}'

    fc.close()
