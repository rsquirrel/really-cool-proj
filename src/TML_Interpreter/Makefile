JFLAGS = 
JC = javac
.SUFFIXES: .java .class
.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = \
        Instruction.java \
        Main.java

default: classes

classes: $(CLASSES:.java=.class)

.PHONY: clean
clean:
	$(RM) *.class
