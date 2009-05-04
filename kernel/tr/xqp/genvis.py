import os

fh = open('LRVisitor.h', 'wb')
fc = open('LRVisitor.cpp', 'wb')

print >>fh, 'class LRVisitor : public ASTVisitor\n{\n'
print >>fc, '#include "LRVisitor.h"\n'

files = os.listdir('./ast/')
files.sort()

for file in files:
    if not file.endswith('.h'):
        continue

    name = file[0:-2]

    if name == 'AST':
        continue

    fn = open(os.path.join('ast/', file), 'rb')

    var_mode = False

    print >>fh, '    void visit(', name, '&n);'
    print >>fc, 'void LRVisitor::visit(', name, '&n)\n{'
    print >>fc, '    lr_str.append("(', name[3:], ' ");'

    for l in fn.readlines():
        if l.startswith('private:'):
            var_mode = True
            continue
        elif l.startswith('public:'):
            break
        elif not var_mode or l.strip() == '':
            continue

        l = l[:-1] # strip \n

        type_name = l.split()[0]
        vars = l.split()[1:]

        if type_name == '//' or not type_name.startswith('AST'):
            continue

        for var in vars:
            if not var.startswith('*'):
                continue

            var = var.strip()
            var = var[1:]

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
