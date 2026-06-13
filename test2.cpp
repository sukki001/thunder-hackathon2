#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <sstream>
#include <functional>
#include <stdexcept>
#include <cmath>
#include <algorithm>

using namespace std;

// ─── Forward declarations ───────────────────────────────────────────────────
struct Value;
using Env = map<string, Value>;
Value evaluate(const string& expr, Env& env);
void executeBlock(const vector<string>& lines, int& i, Env& env);

// ─── Value type ─────────────────────────────────────────────────────────────
struct Value {
    enum Type { NUMBER, STRING, BOOL, NIL } type;
    double num = 0;
    string str;
    bool b = false;

    Value() : type(NIL) {}
    Value(double n) : type(NUMBER), num(n) {}
    Value(const string& s) : type(STRING), str(s) {}
    Value(bool v) : type(BOOL), b(v) {}

    string toString() const {
        if (type == NUMBER) {
            if (num == (long long)num) return to_string((long long)num);
            return to_string(num);
        }
        if (type == STRING) return str;
        if (type == BOOL) return b ? "true" : "false";
        return "undefined";
    }

    double toNumber() const {
        if (type == NUMBER) return num;
        if (type == STRING) { try { return stod(str); } catch(...) { return 0; } }
        if (type == BOOL) return b ? 1 : 0;
        return 0;
    }

    bool toBool() const {
        if (type == NUMBER) return num != 0;
        if (type == STRING) return !str.empty();
        if (type == BOOL) return b;
        return false;
    }
};

// ─── Tokenizer ───────────────────────────────────────────────────────────────
string trim(const string& s) {
    int a = 0, b = (int)s.size() - 1;
    while (a <= b && isspace(s[a])) a++;
    while (b >= a && isspace(s[b])) b--;
    return s.substr(a, b - a + 1);
}

// Find operator outside parentheses/brackets, scanning RIGHT-TO-LEFT for left-assoc
int findOp(const string& expr, const vector<string>& ops) {
    int depth = 0;
    bool inStr = false; char strChar = 0;
    for (int i = (int)expr.size() - 1; i >= 0; i--) {
        char c = expr[i];
        if (!inStr && (c == ')' || c == ']')) depth++;
        else if (!inStr && (c == '(' || c == '[')) depth--;
        else if (!inStr && (c == '\'' || c == '"')) { inStr = true; strChar = c; }
        else if (inStr && c == strChar) inStr = false;
        if (depth != 0 || inStr) continue;
        for (auto& op : ops) {
            int ol = op.size();
            if (i - (int)ol + 1 >= 0 && expr.substr(i - ol + 1, ol) == op) {
                // Make sure it's not part of a longer op
                if (op == "+" || op == "-") {
                    if (i + 1 < (int)expr.size() && (expr[i+1] == '+' || expr[i+1] == '-')) continue;
                    if (i - (int)ol >= 0 && (expr[i-(int)ol] == '+' || expr[i-(int)ol] == '-')) continue;
                }
                return i - ol + 1;
            }
        }
    }
    return -1;
}

// ─── Function storage ────────────────────────────────────────────────────────
struct Function {
    vector<string> params;
    vector<string> body;
};
map<string, Function> functions;

// ─── Expression evaluator ────────────────────────────────────────────────────
Value evaluate(const string& rawExpr, Env& env) {
    string expr = trim(rawExpr);
    if (expr.empty()) return Value();

    // Parenthesized
    if (expr.front() == '(' && expr.back() == ')') {
        int d = 0;
        bool ok = true;
        for (int i = 0; i < (int)expr.size(); i++) {
            if (expr[i] == '(') d++;
            else if (expr[i] == ')') { d--; if (d == 0 && i != (int)expr.size()-1) { ok = false; break; } }
        }
        if (ok) return evaluate(expr.substr(1, expr.size()-2), env);
    }

    // String literal
    if ((expr.front()=='\'' && expr.back()=='\'') || (expr.front()=='"' && expr.back()=='"'))
        return Value(expr.substr(1, expr.size()-2));

    // Boolean / null
    if (expr == "true")  return Value(true);
    if (expr == "false") return Value(false);
    if (expr == "null" || expr == "undefined") return Value();

    // Binary operators (lowest to highest precedence, right-to-left search)
    // Logical
    int p;
    if ((p = findOp(expr, {"||"})) > 0) {
        Value l = evaluate(expr.substr(0, p), env);
        return l.toBool() ? l : evaluate(expr.substr(p+2), env);
    }
    if ((p = findOp(expr, {"&&"})) > 0) {
        Value l = evaluate(expr.substr(0, p), env);
        return !l.toBool() ? l : evaluate(expr.substr(p+2), env);
    }
    // Comparison
    if ((p = findOp(expr, {"==="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toString() == evaluate(expr.substr(p+3),env).toString());
    if ((p = findOp(expr, {"!=="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toString() != evaluate(expr.substr(p+3),env).toString());
    if ((p = findOp(expr, {"=="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toString() == evaluate(expr.substr(p+2),env).toString());
    if ((p = findOp(expr, {"!="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toString() != evaluate(expr.substr(p+2),env).toString());
    if ((p = findOp(expr, {">="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() >= evaluate(expr.substr(p+2),env).toNumber());
    if ((p = findOp(expr, {"<="})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() <= evaluate(expr.substr(p+2),env).toNumber());
    if ((p = findOp(expr, {">"})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() >  evaluate(expr.substr(p+1),env).toNumber());
    if ((p = findOp(expr, {"<"})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() <  evaluate(expr.substr(p+1),env).toNumber());
    // Arithmetic
    if ((p = findOp(expr, {"+"})) > 0) {
        Value l = evaluate(expr.substr(0,p),env), r = evaluate(expr.substr(p+1),env);
        if (l.type==Value::STRING || r.type==Value::STRING) return Value(l.toString()+r.toString());
        return Value(l.toNumber()+r.toNumber());
    }
    if ((p = findOp(expr, {"-"})) > 1)
        return Value(evaluate(expr.substr(0,p),env).toNumber() - evaluate(expr.substr(p+1),env).toNumber());
    if ((p = findOp(expr, {"%"})) > 0)
        return Value(fmod(evaluate(expr.substr(0,p),env).toNumber(), evaluate(expr.substr(p+1),env).toNumber()));
    if ((p = findOp(expr, {"*"})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() * evaluate(expr.substr(p+1),env).toNumber());
    if ((p = findOp(expr, {"/"})) > 0)
        return Value(evaluate(expr.substr(0,p),env).toNumber() / evaluate(expr.substr(p+1),env).toNumber());

    // Unary !
    if (expr.front() == '!') return Value(!evaluate(expr.substr(1), env).toBool());
    // Unary -
    if (expr.front() == '-') return Value(-evaluate(expr.substr(1), env).toNumber());

    // Number literal
    try { size_t pos; double d = stod(expr, &pos); if (pos == expr.size()) return Value(d); } catch(...) {}

    // Function call: name(args)
    if (expr.back() == ')') {
        size_t lp = expr.find('(');
        if (lp != string::npos) {
            string fname = trim(expr.substr(0, lp));
            string argsStr = expr.substr(lp+1, expr.size()-lp-2);

            // Built-ins
            if (fname == "console.log" || fname == "print") {
                // split args by comma (outside parens)
                vector<string> parts; string cur; int d=0;
                for (char c : argsStr) {
                    if (c=='('||c=='[') d++;
                    else if (c==')'||c==']') d--;
                    else if (c==',' && d==0) { parts.push_back(cur); cur=""; continue; }
                    cur+=c;
                }
                parts.push_back(cur);
                string out;
                for (auto& p : parts) { if (!out.empty()) out+=" "; out+=evaluate(p,env).toString(); }
                cout << out << "\n";
                return Value();
            }
            if (fname == "Math.sqrt") return Value(sqrt(evaluate(argsStr,env).toNumber()));
            if (fname == "Math.abs")  return Value(abs(evaluate(argsStr,env).toNumber()));
            if (fname == "Math.pow") {
                size_t c = argsStr.find(',');
                return Value(pow(evaluate(argsStr.substr(0,c),env).toNumber(), evaluate(argsStr.substr(c+1),env).toNumber()));
            }
            if (fname == "Math.floor") return Value(floor(evaluate(argsStr,env).toNumber()));
            if (fname == "Math.ceil")  return Value(ceil(evaluate(argsStr,env).toNumber()));
            if (fname == "Number") return Value(evaluate(argsStr,env).toNumber());
            if (fname == "String") return Value(evaluate(argsStr,env).toString());

            // User-defined function
            if (functions.count(fname)) {
                Function& fn = functions[fname];
                Env localEnv = env;
                // Parse args
                vector<string> argVals; string cur2; int d2=0;
                for (char c : argsStr) {
                    if (c=='('||c=='[') d2++;
                    else if (c==')'||c==']') d2--;
                    else if (c==',' && d2==0) { argVals.push_back(cur2); cur2=""; continue; }
                    cur2+=c;
                }
                if (!trim(cur2).empty()) argVals.push_back(cur2);
                for (int i=0; i<(int)fn.params.size() && i<(int)argVals.size(); i++)
                    localEnv[fn.params[i]] = evaluate(argVals[i], env);
                // Execute body, capture return
                try {
                    int idx = 0;
                    executeBlock(fn.body, idx, localEnv);
                } catch (Value& retVal) { return retVal; }
                return Value();
            }
        }
    }

    // Variable lookup
    if (env.count(expr)) return env[expr];

    return Value(); // undefined
}

// ─── Statement parser helpers ─────────────────────────────────────────────────
vector<string> splitArgs(const string& s) {
    vector<string> parts; string cur; int d=0;
    for (char c : s) {
        if (c=='('||c=='[') d++;
        else if (c==')'||c==']') d--;
        else if (c==',' && d==0) { parts.push_back(trim(cur)); cur=""; continue; }
        cur+=c;
    }
    if (!trim(cur).empty()) parts.push_back(trim(cur));
    return parts;
}

// ─── Block executor ───────────────────────────────────────────────────────────
void executeLine(const string& rawLine, Env& env);

void executeBlock(const vector<string>& lines, int& i, Env& env) {
    while (i < (int)lines.size()) {
        string line = trim(lines[i]);
        if (line == "}" || line == "};") return;
        executeLine(lines[i], env);
        i++;
    }
}

void executeLine(const string& rawLine, Env& env) {
    string line = trim(rawLine);
    if (line.empty() || line == "{") return;
    if (line.back() == ';') line.pop_back();
    line = trim(line);

    // ── return statement
    if (line.substr(0, 6) == "return") {
        string expr = trim(line.substr(6));
        throw evaluate(expr, env);
    }

    // ── Variable declaration: let/const/var x = expr
    if (line.substr(0,3)=="let" || line.substr(0,5)=="const" || line.substr(0,3)=="var") {
        size_t eq = line.find('=');
        if (eq != string::npos) {
            string varPart = line.substr(line.find(' ')+1, eq - line.find(' ') - 1);
            varPart = trim(varPart);
            env[varPart] = evaluate(line.substr(eq+1), env);
        }
        return;
    }

    // ── Assignment / compound assignment: x = / x += / x -= / x *= / x /=
    for (auto& op : vector<string>{"+=","-=","*=","/=","="}) {
        size_t p = line.find(op);
        if (p != string::npos && p > 0) {
            string varName = trim(line.substr(0, p));
            // Make sure it's a simple identifier (no spaces, no parens)
            bool simple = true;
            for (char c : varName) if (!isalnum(c) && c!='_' && c!='.') { simple=false; break; }
            if (!simple) break;
            string rhs = line.substr(p + op.size());
            Value rval = evaluate(rhs, env);
            if (op == "=")  { env[varName] = rval; return; }
            if (op == "+=") { 
                Value l = env[varName];
                if (l.type==Value::STRING || rval.type==Value::STRING) env[varName]=Value(l.toString()+rval.toString());
                else env[varName]=Value(l.toNumber()+rval.toNumber());
                return;
            }
            if (op == "-=") { env[varName] = Value(env[varName].toNumber()-rval.toNumber()); return; }
            if (op == "*=") { env[varName] = Value(env[varName].toNumber()*rval.toNumber()); return; }
            if (op == "/=") { env[varName] = Value(env[varName].toNumber()/rval.toNumber()); return; }
        }
    }

    // ── i++ / i--
    if (line.size()>2 && line.substr(line.size()-2)=="++") { string v=trim(line.substr(0,line.size()-2)); env[v]=Value(env[v].toNumber()+1); return; }
    if (line.size()>2 && line.substr(line.size()-2)=="--") { string v=trim(line.substr(0,line.size()-2)); env[v]=Value(env[v].toNumber()-1); return; }

    // ── function declaration: function name(params) {
    if (line.substr(0,8)=="function") {
        size_t lp=line.find('('), rp=line.find(')');
        string fname=trim(line.substr(8,lp-8));
        string paramStr=line.substr(lp+1,rp-lp-1);
        Function fn;
        for (auto& p : splitArgs(paramStr)) fn.params.push_back(trim(p));
        // Read body lines until matching }
        // We can't do this here without the full lines vector
        // Store a sentinel so the main loop handles it
        functions[fname] = fn; // body filled by main loop
        // Signal to main loop — we embed a special marker
        // Actually we handle this in main loop directly
        return;
    }

    // ── if statement
    if (line.substr(0,2)=="if") {
        size_t lp=line.find('('), rp=line.rfind(')');
        string cond=line.substr(lp+1,rp-lp-1);
        // inline: if (cond) stmt — no braces
        string rest = trim(line.substr(rp+1));
        if (!rest.empty() && rest!="{") { if(evaluate(cond,env).toBool()) executeLine(rest,env); return; }
        return; // brace body handled in main loop
    }

    // ── while / for handled in main loop
    // ── console.log / function calls
    evaluate(line, env);
}

// ─── Main loop ───────────────────────────────────────────────────────────────
int main() {
    cout << "Enter JavaScript (type END on new line to run):\n";
    vector<string> lines;
    string line;
    while (getline(cin, line)) {
        if (trim(line) == "END") break;
        lines.push_back(line);
    }

    Env env;
    int i = 0;

    while (i < (int)lines.size()) {
        string line = trim(lines[i]);

        if (line.empty() || line == "}") { i++; continue; }

        // ── function declaration (multi-line body)
        if (line.substr(0, min((int)line.size(),8)) == "function") {
            size_t lp=line.find('('), rp=line.find(')');
            string fname=trim(line.substr(8,lp-8));
            string paramStr=line.substr(lp+1,rp-lp-1);
            Function fn;
            for (auto& p : splitArgs(paramStr)) if(!trim(p).empty()) fn.params.push_back(trim(p));
            i++; // move past "function name() {"
            int depth=1;
            while (i<(int)lines.size() && depth>0) {
                string bl=trim(lines[i]);
                if (bl=="{" || bl.back()=='{') depth++;
                if (bl=="}" || bl=="};") { depth--; if(depth==0) break; }
                fn.body.push_back(lines[i]);
                i++;
            }
            functions[fname]=fn;
            i++; continue;
        }

        // ── for loop: for (init; cond; update) {
        if (line.substr(0,3)=="for") {
            size_t lp=line.find('('), rp=line.rfind(')');
            string header=line.substr(lp+1,rp-lp-1);
            // split by ;
            vector<string> parts; string cur; int d=0;
            for(char c:header){if(c=='(')d++;else if(c==')')d--;else if(c==';'&&d==0){parts.push_back(cur);cur="";continue;}cur+=c;}
            parts.push_back(cur);
            // Collect body
            i++;
            vector<string> body;
            int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                if(bl=="{")depth++;
                if(bl=="}"||bl=="};"){depth--;if(depth==0)break;}
                body.push_back(lines[i]); i++;
            }
            // Execute
            executeLine(parts[0],env); // init
            while(evaluate(trim(parts[1]),env).toBool()){
                int bi=0;
                try { executeBlock(body,bi,env); } catch(Value& v){ break; }
                executeLine(trim(parts[2]),env); // update
            }
            i++; continue;
        }

        // ── while loop
        if (line.substr(0,5)=="while") {
            size_t lp=line.find('('), rp=line.rfind(')');
            string cond=line.substr(lp+1,rp-lp-1);
            i++;
            vector<string> body; int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                if(bl=="{")depth++;
                if(bl=="}"||bl=="};"){depth--;if(depth==0)break;}
                body.push_back(lines[i]); i++;
            }
            while(evaluate(cond,env).toBool()){
                int bi=0;
                try { executeBlock(body,bi,env); } catch(Value& v){ break; }
            }
            i++; continue;
        }

        // ── if / else
        if (line.substr(0,2)=="if") {
            size_t lp=line.find('('), rp=line.rfind(')');
            string cond=line.substr(lp+1,rp-lp-1);
            bool condVal=evaluate(cond,env).toBool();
            i++;
            vector<string> ifBody,elseBody; int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                if(bl=="{")depth++;
                if(bl=="}"||bl=="};"){depth--;if(depth==0)break;}
                ifBody.push_back(lines[i]); i++;
            }
            i++;
            // Check for else
            if(i<(int)lines.size()&&trim(lines[i]).substr(0,4)=="else"){
                i++;
                depth=1;
                while(i<(int)lines.size()&&depth>0){
                    string bl=trim(lines[i]);
                    if(bl=="{")depth++;
                    if(bl=="}"||bl=="};"){depth--;if(depth==0)break;}
                    elseBody.push_back(lines[i]); i++;
                }
                i++;
            }
            if(condVal){ int bi=0; executeBlock(ifBody,bi,env); }
            else if(!elseBody.empty()){ int bi=0; executeBlock(elseBody,bi,env); }
            continue;
        }

        // Regular statement
        executeLine(lines[i], env);
        i++;
    }
    return 0;
}




function isArmstrong(num) { let temp = num; let sum = 0; 
while (temp > 0) {
    let digit = temp % 10;
    sum += digit *digit*digit;
    temp = Math.floor(temp / 10);
}

return sum == num;

}
console.log(isArmstrong(153));
console.log(isArmstrong(123));




for (let i = 1; i <= 5; i++) { 
 let row = "";

 for (let j = 1; j <= i; j++) {
 row += "*";
 }

    console.log(row); 
}





let num = 7;
if (num % 2 === 0) {
console.log(num + " is Even");
} else {
console.log(num + " is Odd");
}




let arr = [1, 2, 3, 4, 5];
let reversed = [...arr].reverse();
console.log("Original: " + arr.join(", "));
console.log("Reversed: " + reversed.join(", "));





let str = "racecar";
let rev = str.split("").reverse().join("");
if(str == rev){
console.log("is palindrome");
}
else{
console.log("NOT palindrome");
}
END
is palindrome