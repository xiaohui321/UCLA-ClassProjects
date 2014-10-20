/**
 * CS 132
 * Homework 1
 * @author Xiaohui, Zhou
 * ID: 104-014-248
 */
import java.io.*;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

final class Token {
	
	public int type;
	public char value;
	public int line_number;
	public int incrop_direction;
	
	public Token(char v, int t, int line_number, int dir){
		value = v;
		type = t;
		this.line_number= line_number;
		incrop_direction = dir;
	}
	
	public String getStringValue(){
		if(type == Parse.SYMBOL_INCROP){
			String output;
			if(value == '+')
				output = "++";
			else
				output =  "--";
			
			if(incrop_direction == Parse.LEFT)
				return output + '_';
			else
				return '_' + output;
		}else if (type == Parse.LINE_NUM){
			return "\nLINE(" + Integer.toString(line_number) + ")";
		}else if (type == Parse.EOF){
			return "EOF";
		}else{
			return value + "";
		}
	}
}

public class Parse {
	public static final int LEFT = 1;
	public static final int RIGHT = 2;
	
	public static final int SYMBOL_NUM = 0;
	public static final int SYMBOL_REF = 1;
	public static final int SYMBOL_INCROP  = 2;
	public static final int SYMBOL_BINOP = 3;
	public static final int SYMBOL_LEFT_BRACKET = 4;
	public static final int SYMBOL_RIGHT_BRACKET = 5;
	public static final int EOF = 6;
	public static final int SYMBOL_CONC = 7;
	public static final int E = 8;
	
	public static final int EXPR_0 = -1;
	public static final int EXPR_1 = -2;
	public static final int EXPR_2 = -3;
	public static final int LINE_NUM = -4;
	public static final int[][] parseTable = new int[][]{
		 /* NUM,   $,   INC, BIN,   (,   ),  EOF */
		   {  0,   0,     0,   0,   0,   0,   0 },
/*EXPR  */ {  1,   1,     1,   0,   1,   0,   0 },
/*EXPR_1*/ {  5,   6,     7,   0,   8,   0,   0 },
/*EXPR_2*/ {  2,   2,     3,   2,   2,   4,   4 }
		};
	
	public static final int[][] RULES = new int[][] {
		/*1*/{EXPR_0, EXPR_1,              EXPR_2},
		/*2*/{EXPR_2, SYMBOL_BINOP,        EXPR_0,    EXPR_2},
		/*3*/{EXPR_2, SYMBOL_INCROP,       EXPR_2},
		/*4*/{EXPR_2, E},
		/*5*/{EXPR_1, SYMBOL_NUM},
		/*6*/{EXPR_1, SYMBOL_REF,          EXPR_0},
		/*7*/{EXPR_1, SYMBOL_INCROP,       EXPR_0},
		/*8*/{EXPR_1, SYMBOL_LEFT_BRACKET, EXPR_0,    SYMBOL_RIGHT_BRACKET},

	};
	
	public static void main(String[] args) {
		try{
			BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
			String input;
			int line_number = 0;
		    List<Token> tokenList = new LinkedList<Token>();
		    
			while((input = reader.readLine()) != null){
				line_number ++;
				if (!tokenize(tokenList, input, line_number)){
					System.out.println("Parse error in line " + line_number);
					return;
				}
			}

			if(tokenList.size() == line_number){//if empty input
				System.out.println("\nExpression parsed successfully");
				return;
			}
			
			tokenList.add(new Token(' ', EOF,0,0));

			tokenList = parseToken(tokenList);

			tokenList = change_to_postfix(tokenList);
			
			print_outout(tokenList);
			
		}catch(IOException io){
			io.printStackTrace();
		}
	}

	private static void print_outout(List<Token> list) {
		for(int i = 0; i < list.size() - 1; i ++){
			System.out.print(list.get(i).getStringValue() + " ");
		}
		System.out.println(list.get(list.size()-1).getStringValue()
				+ "\nExpression parsed successfully");
	}

	private static List<Token> change_to_postfix(List<Token> list) {
		Stack<Token> stack = new Stack<Token>();
		List<Token> output = new LinkedList<Token>();
		for(int i = 0; i < list.size(); i++){
			Token t = list.get(i);
			if(t.type == EOF){
				break;
			}else if(t.type == SYMBOL_NUM){
				output.add(t);
			}else if(t.type == SYMBOL_LEFT_BRACKET){
				stack.push(t);
			}else if (t.type == SYMBOL_RIGHT_BRACKET){
				while(stack.peek().type != SYMBOL_LEFT_BRACKET){
					output.add(stack.pop());
				}
				stack.pop();
			}else{
				if(stack.empty()){
					stack.push(t);
				}else{
					while(!stack.empty() && stack.peek().type < t.type){
						output.add(stack.pop());
					}
					stack.push(t);
				}
			}
		}
		
		while(!stack.empty()){
			output.add(stack.pop());
		}
		return output;
	}

	private static boolean tokenize(List<Token> tokenList, String input, int line_number){
		char[] inputChars = input.toCharArray();
		//add line number token for displaying when error occurs
		tokenList.add(new Token(' ', LINE_NUM,line_number,0));
		
		for(int i = 0; i < inputChars.length; i++){
			switch (inputChars[i]){
			case ' ':
			case '\t':
				break;
			case '#':
				//Skip the rest of this line
				i = inputChars.length;
				break;
			case '$':
				tokenList.add(new Token(inputChars[i], SYMBOL_REF,0,0));
				break;
			case '(':
				tokenList.add(new Token(inputChars[i], SYMBOL_LEFT_BRACKET,0,0));
				break;
			case ')':
				tokenList.add(new Token(inputChars[i], SYMBOL_RIGHT_BRACKET,0,0));
				break;
			case '+':
				if((i+1)< inputChars.length && inputChars[i + 1] == '+'){
					tokenList.add(new Token(inputChars[i], SYMBOL_INCROP,0,0));
					i++;
				}else{
					tokenList.add(new Token(inputChars[i], SYMBOL_BINOP,0,0));
				}
				break;
			case '-':
				if((i+1)< inputChars.length && inputChars[i + 1] == '-'){
					tokenList.add(new Token(inputChars[i], SYMBOL_INCROP,0,0));
					i++;
				}else{
					tokenList.add(new Token(inputChars[i], SYMBOL_BINOP,0,0));
				}
				break;
			default:
				if (Character.isDigit(inputChars[i])){
					tokenList.add(new Token(inputChars[i], SYMBOL_NUM,0,0));
					break;
				}else{
					return false;
				}
			}
		}
		return true;
	}
	
	private static List<Token> parseToken(List<Token> list){
		List<Token> output = new LinkedList<Token>();
		Stack<Token> stack = new Stack<Token>();
		stack.push(new Token(' ', EOF,0,0));
		stack.push(new Token(' ', EXPR_0,0,0));
		
		int list_index = 0;
		int line_number = 0;
		while(stack.size() > 0){
			Token list_t = list.get(list_index);
			if(list_t.type == LINE_NUM){
				line_number = list_t.line_number;
				list_index ++;
				continue;
			}
			
			Token stack_t = stack.pop();
			while(stack_t.type == E){
				stack_t = stack.pop();
			}
			
			if(stack_t.type >= 0){//a terminal symbol
				if(stack_t.type == list_t.type){//token matched
					list_index ++;
					
					if(list_t.type == EOF){
						output.add(new Token(' ', EOF,0,0));
						return output;
					}else{
						if(stack_t.type == SYMBOL_INCROP){
							if(stack_t.incrop_direction == RIGHT){
								output.add(new Token(list_t.value,SYMBOL_INCROP,0,RIGHT));
							}else{
								output.add(new Token(list_t.value,SYMBOL_INCROP,0,LEFT));
							}
						}else{
							output.add(list_t);
						}
					}
				}else{//unmatched token
					if(stack_t.type == SYMBOL_BINOP && (list_t.type == SYMBOL_NUM || list_t.type == SYMBOL_INCROP || list_t.type == SYMBOL_LEFT_BRACKET || list_t.type == SYMBOL_REF)){
						output.add(new Token('_', SYMBOL_CONC,0,0));
					}else{
						System.out.println("Parse error in line " + line_number);
						System.exit(0);
					}
				}
			}else{//a non-terminal symbol
				int rule_number = parseTable[-stack_t.type][list_t.type];
				if(rule_number == 0){
					System.out.println("Parse error in line " + line_number);
					System.exit(0);
				}
				int[] rule = RULES[rule_number - 1]; 
				
				//push the rule into the stack
				for(int i = rule.length - 1; i > 0; i--){
					if(rule[i] == SYMBOL_INCROP){
						if(rule_number == 3){
							stack.push(new Token(' ',rule[i], 0,RIGHT));
						}else if(rule_number == 7){
							stack.push(new Token(' ',rule[i], 0,LEFT));
						}
					}else{
						stack.push(new Token(' ',rule[i], 0,0));
					}
				}
			}
		}
		return null;
	}
}
