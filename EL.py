symbol_list = set()
tbox = set()


# 定义概念、Role等
class Concept(object):
    def __init__(self, concept_type):
        self.type = concept_type


class Top(Concept):  # 全集
    def __init__(self):
        super().__init__("top")
        self.name = "T"

    def __str__(self):
        return str(self.name)


class Role(Concept):
    def __init__(self, name):
        super().__init__("role")
        self.name = name

    def __str__(self):
        return str(self.name)


class AtomConcept(Concept):  # 原子概念
    def __init__(self, name):
        super().__init__("atomic concept")
        self.name = name
        symbol_list.add(self.name)

    def __str__(self):
        return str(self.name)


class Exist(Concept):  # 存在
    def __init__(self, c21, c22):
        super().__init__("complex_concepts")
        self.A = c21
        self.B = c22
        self.atom_type = "Exist"
        self.name = "∃" + str(self.A) + "." + str(self.B)

    def __str__(self):
        return str(self.name)


class Include(Concept):  # 包含
    def __init__(self, c31, c32):
        super().__init__("complex_concepts")
        self.atom_type = "include"
        self.A = c31
        self.B = c32
        self.name = str(self.A) + " ⊑ " + str(self.B)

    def __str__(self):
        return str(self.name)


class Conjunction(Concept):  # 合取
    def __init__(self, c11, c12):
        super().__init__("complex_concepts")
        self.atom_type = "conjunction"
        self.A = c11
        self.B = c12
        self.name = str(self.A) + " ⊓ " + str(self.B)

    def __str__(self):
        return str(self.name)


class Reasoner:  # 推理是否A belong to D
    def __init__(self, include_atom):
        self.include_atom = include_atom
        self.left_temp, self.right_temp = include_atom.A, include_atom.B  # A ⊑ D
        self.S = {}  # S(X)
        self.slist = list(symbol_list)  # 存放atomic concept的list
        self.R = [[[] for i in self.slist] for j in self.slist]  # R(X,Y)

    def fill(self):
        for i in self.slist:
            self.S[i] = [Top().name, i]
        for atom in tbox:
            if atom.type is "complex_concepts" and atom.atom_type is "include" and atom.B.type is not "complex_concepts":
                if self.S.get(atom.A.name) is None:
                    self.S[atom.A.name] = [Top().name, atom.A.name, atom.B.name]
                else:
                    self.S[atom.A.name].append(atom.B.name)
            elif atom.type is "complex_concepts" and atom.atom_type is "include" and atom.B.type is "complex_concepts" and atom.B.atom_type is "Exist":
                i, j = self.slist.index(atom.A.name), self.slist.index(atom.B.B.name)
                self.R[i][j].append(atom.B.A.name)
                self.R[j][i].append(atom.B.A.name)
        for n in range(10):  # 对TBox 不断使用Completion Rules
            #  completion rule1
            for m in self.S.keys():
                for atom in tbox:
                    if atom.type is "complex_concepts" and atom.atom_type is "include" and atom.A.type is "complex_concepts" and atom.A.atom_type is "conjunction":
                        if self.S[m].__contains__(atom.A.A.name) and self.S[m].__contains__(atom.A.B.name) and not \
                                self.S[
                                    m].__contains__(atom.B.name):
                            self.S[m].append(atom.B.name)
            #  completion rule2
            for m in self.slist:
                for atom in tbox:
                    if atom.type is "complex_concepts" and atom.atom_type is "include" and atom.B.type is "complex_concepts" and atom.B.atom_type is "Exist":
                        i, j = self.slist.index(atom.A.name), self.slist.index(atom.B.B.name)
                        if not len(self.R[i][j]) == 0 and self.S[m].__contains__(self.slist) and not \
                        self.R[self.slist.index(m)][j].__contains__(atom.B.A.name):
                            self.R[self.slist.index(m)][j].append(atom.B.A.name)
                            self.R[j][self.slist.index(m)].append(atom.B.A.name)
            #  completion rule3
            for m in self.slist:
                for atom in tbox:
                    if atom.type is "complex_concepts" and atom.atom_type is "include" and atom.B.type is "complex_concepts" and atom.B.atom_type is "Exist" and m == atom.A.name:
                        r, Y = atom.B.A.name, atom.B.B.name
                        for A in self.S[Y]:
                            for atom in tbox:
                                if atom.type is "complex_concepts" and atom.atom_type is "include" and atom.A.type is "complex_concepts" and atom.A.atom_type is "Exist" and A == atom.A.B.name and r == atom.A.A.name:
                                    B = atom.B.name
                                    if not self.S[m].__contains__(B):
                                        self.S[m].append(B)

    def show_str(self):   # 打印S(A)
        print("self.S(" + self.left_temp.name + "): " + str(self.S[self.left_temp.name]))
        if self.S[self.left_temp.name].__contains__(self.right_temp.name):
            print(str(self.include_atom) + " 成立 ")
        else:
            print(str(self.include_atom) + " 不成立 ")


def get_symbol():  # 获取new atomic concept A1，A2....
    temp = 1
    while True:
        A = "A" + str(temp)
        temp += 1
        if not symbol_list.__contains__(A):
            break

    return A


def axiom_adder(atom):
    tbox.add(atom)


def add_atoms(atom_list):  # 添加原始TBox
    for atom in atom_list:
        axiom_adder(atom)


def show_res():  # 打印所有TBox
    temp_str = "TBox: \n"
    for a in tbox:
        temp_str += a.__str__() + "\n"
    print(temp_str)


def main():
    add_atoms(
        [Include(AtomConcept("A"), Conjunction(AtomConcept("B"), Exist(Role("r"), AtomConcept("C")))),
         Include(AtomConcept("C"), Exist(Role("s"), AtomConcept("D"))),
         Include(Conjunction(Exist(Role("r"), Exist(Role("s"), Top())), AtomConcept("B")),
                 AtomConcept("D"))])
    print("原始TBox:-------------------------------------------------------------")
    show_res()
    print("开始Normalization:----------------------------------------------------")
    normalization()
    print("开始推理:--------------------------------------------------------------")
    res = Reasoner(Include(AtomConcept("A"), AtomConcept("D")))
    res.fill()
    res.show_str()


def normalization():  # Normalization所有TBox
    flag = True
    while flag:  # 对三条TBox使用NF2~6循环进行normalization，直到每条TBox中只剩atomic概念停下。
        flag = False
        for atom in list(tbox):
            if atom.type is "complex_concepts":
                # NF2
                if atom.atom_type is "include" and atom.A.type is "complex_concepts" and atom.B.type is "complex_concepts":
                    A, B = atom.A, atom.B
                    C = AtomConcept(get_symbol())
                    tbox.remove(atom)
                    add_atoms([Include(A, C), Include(C, B)])
                    flag = True
                # NF3
                elif atom.atom_type is "include" and atom.A.type is "complex_concepts" and atom.A.atom_type is "Exist" and atom.A.B.type is "complex_concepts":

                    R, C, D = atom.A.A, atom.A.B, atom.B
                    A = AtomConcept(get_symbol())
                    tbox.remove(atom)
                    add_atoms([Include(C, A), Include(Exist(R, A), D)])
                    flag = True
                # NF4
                elif atom.atom_type is "include" and atom.A.type is "complex_concepts" and atom.A.atom_type is "conjunction" and atom.A.A.type is "complex_concepts":
                    A1, A2, B = atom.A.A, atom.A.B, atom.B
                    C = AtomConcept(get_symbol())
                    tbox.remove(atom)
                    add_atoms([Include(A1, C), Include(Conjunction(C, A2), B)])
                    flag = True
                # NF5
                elif atom.atom_type is "include" and atom.B.type is "complex_concepts" and atom.B.atom_type is "Exist" and atom.B.B.type is "complex_concepts":
                    B, R, C = atom.A, atom.B.A, atom.B.B
                    A = AtomConcept(get_symbol())
                    tbox.remove(atom)
                    add_atoms([Include(C, A), Include(B, Exist(R, A))])
                    flag = True
                # NF6
                elif atom.atom_type is "include" and atom.B.type is "complex_concepts" and atom.B.atom_type is "conjunction":
                    A, B1, B2 = atom.A, atom.B.A, atom.B.B
                    tbox.remove(atom)
                    add_atoms([Include(A, B1), Include(A, B2)])
                    flag = True
    show_res()


if __name__ == '__main__':
    main()
