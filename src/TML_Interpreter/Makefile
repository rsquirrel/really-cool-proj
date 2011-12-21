JFLAGS =
JC = javac
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
		TreeIterator.java \
		PreorderIterator.java \
		InorderIterator.java \
		PostorderIterator.java \
		LevelorderIterator.java \
        TMLTree.java \
        Instruction.java \
        Program.java \
        Main.java

default: classes

classes: $(CLASSES:.java=.class)

.PHONY: clean
clean:
	$(RM) *.class
