package analysis;

import syntaxtree.*;
import visitor.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class TypeCheck {
    public static void main(String[] Args) {
        File testDir = new File("../tests/"); // create a folder called tests, and put the tests in it
        MiniJavaParser parse = null;
        for (final File fileEntry : testDir.listFiles()) {
            if (fileEntry.isFile()) {
                FileInputStream in = null;
                try {
                    in = new FileInputStream(fileEntry.getAbsoluteFile());
                }
                catch (FileNotFoundException err) {
                    err.printStackTrace();
                }

                try {
                    System.out.println("\n\nProcessing: " + fileEntry.getName());
                    if (null == parse) {
                        parse = new MiniJavaParser(in);
                    }
                    else {
                        parse.ReInit(in);
                    }
                    
                    // put your logic here	
                    Node root = MiniJavaParser.Goal();
                    //System.out.println("Program parsed successfully");
                    
                    FirstRoundVisitor visitor1= new FirstRoundVisitor();
                    root.accept(visitor1);
                    
                    GJDepthFirst<SymbolTable.Type,Object> visitor2 = new SecondRoundVisitor(visitor1.getsymbolTable());
                    root.accept(visitor2,null);
                    
                    System.out.println("Program type checked successfully");
                    
                }
                catch (ParseException e) {
                    System.out.println(e.toString());
                }
                catch (Error e1) {
                    System.out.println(e1.toString());
                }
            }
        }
    }
}
/*
import syntaxtree.*;
import visitor.*;

public class Typecheck {
	public static void main(String [] args) {
	   try {
           new MiniJavaParser(System.in);
           Node root = MiniJavaParser.Goal();
           System.out.println("Program parsed successfully");
           root.accept(new GJNoArguDepthFirst());
           System.out.println("Program type checked successfully");
        }
        catch (ParseException e) {
           System.out.println(e.toString());
        }
        catch (Error e1) {
           System.out.println("Type Error");
        }
   }
}




*/