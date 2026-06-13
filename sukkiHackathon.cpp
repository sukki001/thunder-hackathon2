#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <cmath>
#include <algorithm>
#include <memory>

using namespace std;

// ═══════════════════════════════════════════════════════════
//  VALUE SYSTEM
// ═══════════════════════════════════════════════════════════
struct Value {
    enum Type { NUMBER, STRING, BOOL, NIL, ARRAY } type = NIL;
    double num = 0;
    string str;
    bool b = false;
    shared_ptr<vector<Value>> arr;

    Value() : type(NIL) {}
    Value(double n) : type(NUMBER), num(n) {}
    Value(const string& s) : type(STRING), str(s) {}
    Value(bool v) : type(BOOL), b(v) {}
    Value(shared_ptr<vector<Value>> a) : type(ARRAY), arr(a) {}

    string toString() const {
        if (type == NUMBER) {
            if (num == (long long)num) return to_string((long long)num);
            ostringstream oss; oss << num; return oss.str();
        }
        if (type == STRING) return str;
        if (type == BOOL)   return b ? "true" : "false";
        if (type == ARRAY) {
            string s;
            for (int i = 0; i < (int)arr->size(); i++) {
                if (i) s += ",";
                s += (*arr)[i].toString();
            }
            return s;
        }
        return "undefined";
    }

    double toNumber() const {
        if (type == NUMBER) return num;
        if (type == STRING) { try { return stod(str); } catch(...) { return 0; } }
        if (type == BOOL)   return b ? 1 : 0;
        return 0;
    }

    bool toBool() const {
        if (type == NUMBER) return num != 0;
        if (type == STRING) return !str.empty();
        if (type == BOOL)   return b;
        if (type == ARRAY)  return true;
        return false;
    }
};

// ═══════════════════════════════════════════════════════════
//  GLOBALS
// ═══════════════════════════════════════════════════════════
using Env = map<string, Value>;

struct Function { vector<string> params; vector<string> body; };
map<string, Function> functions;

// Return exception
struct ReturnException { Value val; };

// ═══════════════════════════════════════════════════════════
//  UTILITIES
// ═══════════════════════════════════════════════════════════
string trim(const string& s) {
    int a = 0, b = (int)s.size() - 1;
    while (a <= b && isspace((unsigned char)s[a])) a++;
    while (b >= a && isspace((unsigned char)s[b])) b--;
    return (a > b) ? "" : s.substr(a, b - a + 1);
}

// Split by comma respecting parens/brackets/quotes
vector<string> splitByComma(const string& s) {
    vector<string> parts; string cur; int d = 0; bool inStr = false; char sc = 0;
    for (char c : s) {
        if (!inStr && (c == '\'' || c == '"')) { inStr = true; sc = c; cur += c; }
        else if (inStr && c == sc) { inStr = false; cur += c; }
        else if (!inStr && (c == '(' || c == '[' || c == '{')) { d++; cur += c; }
        else if (!inStr && (c == ')' || c == ']' || c == '}')) { d--; cur += c; }
        else if (!inStr && d == 0 && c == ',') { parts.push_back(trim(cur)); cur = ""; }
        else cur += c;
    }
    string t = trim(cur); if (!t.empty()) parts.push_back(t);
    return parts;
}

// Find operator outside parens/quotes scanning right-to-left
int findOp(const string& expr, const string& op) {
    int depth = 0; bool inStr = false; char sc = 0;
    int ol = op.size();
    for (int i = (int)expr.size() - 1; i >= ol - 1; i--) {
        char c = expr[i];
        if (!inStr && (c == ')' || c == ']')) depth++;
        else if (!inStr && (c == '(' || c == '[')) depth--;
        else if (!inStr && (c == '\'' || c == '"')) { inStr = true; sc = c; }
        else if (inStr && c == sc) inStr = false;
        if (depth != 0 || inStr) continue;
        if (i - ol + 1 >= 0 && expr.substr(i - ol + 1, ol) == op) {
            int pos = i - ol + 1;
            // Avoid mismatches like != vs !==, == vs ===, etc.
            if (op == "=" && pos > 0 && string("!<>=+-*/%&|").find(expr[pos-1]) != string::npos) continue;
            if (op == "=" && pos + 1 < (int)expr.size() && expr[pos+1] == '=') continue;
            if (op == ">" && pos > 0 && expr[pos-1] == '=') continue;
            if (op == "<" && pos > 0 && expr[pos-1] == '=') continue;
            if (op == "+" && pos > 0 && expr[pos-1] == '+') continue;
            if (op == "+" && pos+1 < (int)expr.size() && expr[pos+1] == '+') continue;
            if (op == "-" && pos > 0 && expr[pos-1] == '-') continue;
            if (op == "-" && pos+1 < (int)expr.size() && expr[pos+1] == '-') continue;
            if (op == "*" && pos > 0 && expr[pos-1] == '*') continue;
            if (op == "*" && pos+1 < (int)expr.size() && expr[pos+1] == '*') continue;
            if (op == "/" && pos > 0 && expr[pos-1] == '/') continue;
            if (op == "&" && (pos > 0 && expr[pos-1] == '&')) continue;
            if (op == "|" && (pos > 0 && expr[pos-1] == '|')) continue;
            return pos;
        }
    }
    return -1;
}

// ═══════════════════════════════════════════════════════════
//  FORWARD DECLARATIONS
// ═══════════════════════════════════════════════════════════
Value evaluate(const string& rawExpr, Env& env);
void executeLine(const string& rawLine, Env& env);
void executeLines(const vector<string>& lines, Env& env);

// ═══════════════════════════════════════════════════════════
//  ARRAY / STRING METHOD CALLS
// ═══════════════════════════════════════════════════════════
// Handle: expr.method(args)  — returns {true, value} if handled
pair<bool,Value> tryMethodCall(const string& expr, Env& env) {
    // Find last .method( that isn't inside brackets/parens
    if (expr.back() != ')') return {false, Value()};

    // Walk backwards to find the closing ) match
    int depth = 0; bool inStr = false; char sc = 0;
    int argsEnd = (int)expr.size() - 1; // position of ')'

    // Find matching '('
    int argsStart = -1;
    depth = 1;
    for (int i = argsEnd - 1; i >= 0; i--) {
        char c = expr[i];
        if (!inStr && (c == '\'' || c == '"')) { inStr = true; sc = c; }
        else if (inStr && c == sc) inStr = false;
        if (!inStr) {
            if (c == ')') depth++;
            else if (c == '(') { depth--; if (depth == 0) { argsStart = i; break; } }
        }
    }
    if (argsStart <= 0) return {false, Value()};

    // Find the dot before method name
    int dotPos = -1;
    for (int i = argsStart - 1; i >= 0; i--) {
        if (expr[i] == '.') { dotPos = i; break; }
        if (!isalnum(expr[i]) && expr[i] != '_') break;
    }
    if (dotPos < 0) return {false, Value()};

    string objExpr   = expr.substr(0, dotPos);
    string method    = expr.substr(dotPos + 1, argsStart - dotPos - 1);
    string argsStr   = expr.substr(argsStart + 1, argsEnd - argsStart - 1);
    vector<string> argParts = splitByComma(argsStr);

    Value obj = evaluate(objExpr, env);

    // ── Array methods ──────────────────────────────────────
    if (obj.type == Value::ARRAY) {
        if (method == "reverse") {
            auto newArr = make_shared<vector<Value>>(*obj.arr);
            reverse(newArr->begin(), newArr->end());
            return {true, Value(newArr)};
        }
        if (method == "join") {
            string sep = argParts.empty() ? "," : evaluate(argParts[0], env).toString();
            string result;
            for (int i = 0; i < (int)obj.arr->size(); i++) {
                if (i) result += sep;
                result += (*obj.arr)[i].toString();
            }
            return {true, Value(result)};
        }
        if (method == "push") {
            for (auto& a : argParts) obj.arr->push_back(evaluate(a, env));
            return {true, Value((double)obj.arr->size())};
        }
        if (method == "pop") {
            if (!obj.arr->empty()) { Value v = obj.arr->back(); obj.arr->pop_back(); return {true, v}; }
            return {true, Value()};
        }
        if (method == "indexOf") {
            Value target = evaluate(argParts[0], env);
            for (int i = 0; i < (int)obj.arr->size(); i++)
                if ((*obj.arr)[i].toString() == target.toString()) return {true, Value((double)i)};
            return {true, Value(-1.0)};
        }
        if (method == "includes") {
            Value target = evaluate(argParts[0], env);
            for (auto& v : *obj.arr)
                if (v.toString() == target.toString()) return {true, Value(true)};
            return {true, Value(false)};
        }
        if (method == "slice") {
            int from = argParts.size() > 0 ? (int)evaluate(argParts[0], env).toNumber() : 0;
            int to   = argParts.size() > 1 ? (int)evaluate(argParts[1], env).toNumber() : (int)obj.arr->size();
            if (from < 0) from = max(0, (int)obj.arr->size() + from);
            if (to   < 0) to   = max(0, (int)obj.arr->size() + to);
            to = min(to, (int)obj.arr->size());
            auto newArr = make_shared<vector<Value>>(obj.arr->begin() + from, obj.arr->begin() + to);
            return {true, Value(newArr)};
        }
        if (method == "concat") {
            auto newArr = make_shared<vector<Value>>(*obj.arr);
            for (auto& a : argParts) {
                Value v = evaluate(a, env);
                if (v.type == Value::ARRAY) for (auto& x : *v.arr) newArr->push_back(x);
                else newArr->push_back(v);
            }
            return {true, Value(newArr)};
        }
        if (method == "length") return {true, Value((double)obj.arr->size())}; // treated as prop
    }

    // ── String methods ─────────────────────────────────────
    if (obj.type == Value::STRING) {
        if (method == "split") {
            string sep = argParts.empty() ? "" : evaluate(argParts[0], env).toString();
            auto newArr = make_shared<vector<Value>>();
            if (sep.empty()) { for (char c : obj.str) newArr->push_back(Value(string(1,c))); }
            else {
                size_t pos = 0, found;
                while ((found = obj.str.find(sep, pos)) != string::npos) {
                    newArr->push_back(Value(obj.str.substr(pos, found - pos)));
                    pos = found + sep.size();
                }
                newArr->push_back(Value(obj.str.substr(pos)));
            }
            return {true, Value(newArr)};
        }
        if (method == "join") { return {true, obj}; } // fallback
        if (method == "reverse") { string r = obj.str; reverse(r.begin(),r.end()); return {true, Value(r)}; }
        if (method == "toUpperCase") { string r=obj.str; transform(r.begin(),r.end(),r.begin(),::toupper); return {true,Value(r)}; }
        if (method == "toLowerCase") { string r=obj.str; transform(r.begin(),r.end(),r.begin(),::tolower); return {true,Value(r)}; }
        if (method == "trim") { return {true, Value(trim(obj.str))}; }
        if (method == "length") { return {true, Value((double)obj.str.size())}; }
        if (method == "includes") {
            string needle = evaluate(argParts[0], env).toString();
            return {true, Value(obj.str.find(needle) != string::npos)};
        }
        if (method == "indexOf") {
            string needle = evaluate(argParts[0], env).toString();
            size_t f = obj.str.find(needle);
            return {true, f == string::npos ? Value(-1.0) : Value((double)f)};
        }
        if (method == "slice" || method == "substring") {
            int from = argParts.size() > 0 ? (int)evaluate(argParts[0], env).toNumber() : 0;
            int to   = argParts.size() > 1 ? (int)evaluate(argParts[1], env).toNumber() : (int)obj.str.size();
            if (from < 0) from = max(0, (int)obj.str.size() + from);
            to = min(to, (int)obj.str.size());
            return {true, Value(obj.str.substr(from, to - from))};
        }
        if (method == "charAt") {
            int idx = (int)evaluate(argParts[0], env).toNumber();
            if (idx >= 0 && idx < (int)obj.str.size()) return {true, Value(string(1, obj.str[idx]))};
            return {true, Value(string(""))};
        }
        if (method == "replace") {
            string from = evaluate(argParts[0], env).toString();
            string to   = evaluate(argParts[1], env).toString();
            string result = obj.str;
            size_t pos = result.find(from);
            if (pos != string::npos) result.replace(pos, from.size(), to);
            return {true, Value(result)};
        }
        if (method == "repeat") {
            int n = (int)evaluate(argParts[0], env).toNumber();
            string result; for (int i = 0; i < n; i++) result += obj.str;
            return {true, Value(result)};
        }
        if (method == "startsWith") {
            string prefix = evaluate(argParts[0], env).toString();
            return {true, Value(obj.str.substr(0, prefix.size()) == prefix)};
        }
        if (method == "endsWith") {
            string suffix = evaluate(argParts[0], env).toString();
            if (suffix.size() > obj.str.size()) return {true, Value(false)};
            return {true, Value(obj.str.substr(obj.str.size()-suffix.size()) == suffix)};
        }
    }
    return {false, Value()};
}

// ═══════════════════════════════════════════════════════════
//  EXPRESSION EVALUATOR
// ═══════════════════════════════════════════════════════════
Value evaluate(const string& rawExpr, Env& env) {
    string expr = trim(rawExpr);
    if (expr.empty()) return Value();

    // ── Parentheses wrap ──────────────────────────────────
    if (expr.front() == '(' && expr.back() == ')') {
        int d = 0; bool ok = true; bool inStr = false; char sc = 0;
        for (int i = 0; i < (int)expr.size(); i++) {
            char c = expr[i];
            if (!inStr && (c=='\''||c=='"')) { inStr=true; sc=c; }
            else if (inStr && c==sc) inStr=false;
            if (!inStr) {
                if (c == '(') d++;
                else if (c == ')') { d--; if (d == 0 && i != (int)expr.size()-1) { ok=false; break; } }
            }
        }
        if (ok) return evaluate(expr.substr(1, expr.size()-2), env);
    }

    // ── String literals ───────────────────────────────────
    if ((expr.front()=='\''&&expr.back()=='\'')||(expr.front()=='"'&&expr.back()=='"'))
        return Value(expr.substr(1, expr.size()-2));

    // ── Boolean / null ────────────────────────────────────
    if (expr=="true")  return Value(true);
    if (expr=="false") return Value(false);
    if (expr=="null"||expr=="undefined") return Value();

    // ── Array literal: [a, b, c] ──────────────────────────
    if (expr.front()=='[' && expr.back()==']') {
        string inner = expr.substr(1, expr.size()-2);
        auto arr = make_shared<vector<Value>>();
        // Spread: [...varName]
        string t = trim(inner);
        if (t.substr(0,3) == "...") {
            Value src = evaluate(t.substr(3), env);
            if (src.type == Value::ARRAY) *arr = *src.arr;
            return Value(arr);
        }
        vector<string> parts = splitByComma(inner);
        for (auto& p : parts) {
            string pt = trim(p);
            if (pt.substr(0,3) == "...") {
                Value src = evaluate(pt.substr(3), env);
                if (src.type == Value::ARRAY) for (auto& v : *src.arr) arr->push_back(v);
            } else if (!pt.empty()) {
                arr->push_back(evaluate(pt, env));
            }
        }
        return Value(arr);
    }

    // ── Binary operators (low → high precedence) ──────────
    int p;

    // Logical OR / AND
    if ((p=findOp(expr,"||")) > 0) { Value l=evaluate(expr.substr(0,p),env); return l.toBool()?l:evaluate(expr.substr(p+2),env); }
    if ((p=findOp(expr,"&&")) > 0) { Value l=evaluate(expr.substr(0,p),env); return !l.toBool()?l:evaluate(expr.substr(p+2),env); }

    // Comparison (check longer ops first)
    if ((p=findOp(expr,"==="))>0) return Value(evaluate(expr.substr(0,p),env).toString()==evaluate(expr.substr(p+3),env).toString());
    if ((p=findOp(expr,"!=="))>0) return Value(evaluate(expr.substr(0,p),env).toString()!=evaluate(expr.substr(p+3),env).toString());
    if ((p=findOp(expr,"==")) >0) return Value(evaluate(expr.substr(0,p),env).toString()==evaluate(expr.substr(p+2),env).toString());
    if ((p=findOp(expr,"!=")) >0) return Value(evaluate(expr.substr(0,p),env).toString()!=evaluate(expr.substr(p+2),env).toString());
    if ((p=findOp(expr,">=")) >0) return Value(evaluate(expr.substr(0,p),env).toNumber()>=evaluate(expr.substr(p+2),env).toNumber());
    if ((p=findOp(expr,"<=")) >0) return Value(evaluate(expr.substr(0,p),env).toNumber()<=evaluate(expr.substr(p+2),env).toNumber());
    if ((p=findOp(expr,">"))  >0) return Value(evaluate(expr.substr(0,p),env).toNumber()> evaluate(expr.substr(p+1),env).toNumber());
    if ((p=findOp(expr,"<"))  >0) return Value(evaluate(expr.substr(0,p),env).toNumber()< evaluate(expr.substr(p+1),env).toNumber());

    // Addition / subtraction
    if ((p=findOp(expr,"+"))>0) {
        Value l=evaluate(expr.substr(0,p),env), r=evaluate(expr.substr(p+1),env);
        if (l.type==Value::STRING||r.type==Value::STRING) return Value(l.toString()+r.toString());
        return Value(l.toNumber()+r.toNumber());
    }
    if ((p=findOp(expr,"-"))>1) return Value(evaluate(expr.substr(0,p),env).toNumber()-evaluate(expr.substr(p+1),env).toNumber());

    // Multiplication / division / modulo / power
    if ((p=findOp(expr,"%"))>0) return Value(fmod(evaluate(expr.substr(0,p),env).toNumber(),evaluate(expr.substr(p+1),env).toNumber()));
    if ((p=findOp(expr,"**"))>0) return Value(pow(evaluate(expr.substr(0,p),env).toNumber(),evaluate(expr.substr(p+2),env).toNumber()));
    if ((p=findOp(expr,"*"))>0) return Value(evaluate(expr.substr(0,p),env).toNumber()*evaluate(expr.substr(p+1),env).toNumber());
    if ((p=findOp(expr,"/"))>0) return Value(evaluate(expr.substr(0,p),env).toNumber()/evaluate(expr.substr(p+1),env).toNumber());

    // ── Unary operators ───────────────────────────────────
    if (expr.front()=='!') return Value(!evaluate(expr.substr(1),env).toBool());
    if (expr.front()=='-' && expr.size()>1) return Value(-evaluate(expr.substr(1),env).toNumber());

    // ── Number literal ────────────────────────────────────
    try { size_t pos; double d=stod(expr,&pos); if(pos==expr.size()) return Value(d); } catch(...) {}

    // ── .length property (no parens) ─────────────────────
    if (expr.size()>7 && expr.substr(expr.size()-7)==".length") {
        Value obj = evaluate(expr.substr(0, expr.size()-7), env);
        if (obj.type==Value::ARRAY)  return Value((double)obj.arr->size());
        if (obj.type==Value::STRING) return Value((double)obj.str.size());
    }

    // ── Array index: arr[i] ───────────────────────────────
    if (expr.back()==']') {
        size_t lb = expr.rfind('[');
        if (lb!=string::npos) {
            string arrExpr = expr.substr(0,lb);
            int idx = (int)evaluate(expr.substr(lb+1,expr.size()-lb-2),env).toNumber();
            Value arrVal = evaluate(arrExpr,env);
            if (arrVal.type==Value::ARRAY && idx>=0 && idx<(int)arrVal.arr->size())
                return (*arrVal.arr)[idx];
            if (arrVal.type==Value::STRING && idx>=0 && idx<(int)arrVal.str.size())
                return Value(string(1,arrVal.str[idx]));
        }
    }

    // ── Method call with chaining: obj.method(args) ───────
    if (expr.back()==')') {
        pair<bool,Value> methodResult = tryMethodCall(expr, env);
        if (methodResult.first) return methodResult.second;

        // ── Built-in functions ────────────────────────────
        size_t lp = string::npos;
        { int d=0; bool inStr=false; char sc=0;
          for(int i=(int)expr.size()-1;i>=0;i--){
            char c=expr[i];
            if(!inStr&&(c=='\''||c=='"')){inStr=true;sc=c;}
            else if(inStr&&c==sc)inStr=false;
            if(!inStr){if(c==')')d++;else if(c=='('){d--;if(d==0){lp=i;break;}}}
          }
        }
        if (lp!=string::npos) {
            string fname = trim(expr.substr(0,lp));
            string argsStr = expr.substr(lp+1,expr.size()-lp-2);

            if (fname=="console.log"||fname=="print") {
                vector<string> parts = splitByComma(argsStr);
                string out;
                for(auto& pt:parts){ if(!out.empty())out+=" "; out+=evaluate(pt,env).toString(); }
                cout << out << "\n";
                return Value();
            }
            if (fname=="Math.sqrt")  return Value(sqrt(evaluate(argsStr,env).toNumber()));
            if (fname=="Math.abs")   return Value(fabs(evaluate(argsStr,env).toNumber()));
            if (fname=="Math.floor") return Value(floor(evaluate(argsStr,env).toNumber()));
            if (fname=="Math.ceil")  return Value(ceil(evaluate(argsStr,env).toNumber()));
            if (fname=="Math.round") return Value(round(evaluate(argsStr,env).toNumber()));
            if (fname=="Math.max") {
                auto a=splitByComma(argsStr); double m=evaluate(a[0],env).toNumber();
                for(auto& x:a) m=max(m,evaluate(x,env).toNumber()); return Value(m);
            }
            if (fname=="Math.min") {
                auto a=splitByComma(argsStr); double m=evaluate(a[0],env).toNumber();
                for(auto& x:a) m=min(m,evaluate(x,env).toNumber()); return Value(m);
            }
            if (fname=="Math.pow") {
                auto a=splitByComma(argsStr);
                return Value(pow(evaluate(a[0],env).toNumber(),evaluate(a[1],env).toNumber()));
            }
            if (fname=="Number") return Value(evaluate(argsStr,env).toNumber());
            if (fname=="String") return Value(evaluate(argsStr,env).toString());
            if (fname=="parseInt") {
                auto a=splitByComma(argsStr);
                return Value((double)(long long)evaluate(a[0],env).toNumber());
            }
            if (fname=="parseFloat") return Value(evaluate(argsStr,env).toNumber());
            if (fname=="isNaN") {
                try { stod(evaluate(argsStr,env).toString()); return Value(false); } catch(...){ return Value(true); }
            }

            // ── User-defined function call ─────────────────
            if (functions.count(fname)) {
                Function& fn = functions[fname];
                Env localEnv = env;
                vector<string> argParts = splitByComma(argsStr);
                for (int i=0; i<(int)fn.params.size()&&i<(int)argParts.size(); i++)
                    localEnv[fn.params[i]] = evaluate(argParts[i], env);
                try { executeLines(fn.body, localEnv); }
                catch (ReturnException& r) { return r.val; }
                return Value();
            }
        }
    }

    // ── Variable lookup ───────────────────────────────────
    if (env.count(expr)) return env[expr];

    return Value(); // undefined
}

// ═══════════════════════════════════════════════════════════
//  STATEMENT EXECUTOR
// ═══════════════════════════════════════════════════════════
void executeLine(const string& rawLine, Env& env) {
    string line = trim(rawLine);
    if (line.empty() || line == "{" || line == "}") return;
    if (!line.empty() && line.back()==';') line.pop_back();
    line = trim(line);
    if (line.empty()) return;

    // return
    if (line.size()>=6 && line.substr(0,6)=="return") {
        string expr = trim(line.substr(6));
        throw ReturnException{evaluate(expr,env)};
    }

    // let / const / var
    if (line.substr(0,3)=="let"||line.substr(0,5)=="const"||line.substr(0,3)=="var") {
        size_t eq = line.find('=');
        if (eq!=string::npos) {
            string kw   = line.substr(0,3)=="con" ? "const" : (line.substr(0,3)=="let"?"let":"var");
            string rest = line.substr(kw.size());
            string varPart = trim(rest.substr(0, rest.find('=')));
            string valPart = trim(rest.substr(rest.find('=')+1));
            env[varPart] = evaluate(valPart,env);
        }
        return;
    }

    // i++ / i--
    if (line.size()>2&&line.substr(line.size()-2)=="++") { string v=trim(line.substr(0,line.size()-2)); if(env.count(v))env[v]=Value(env[v].toNumber()+1); return; }
    if (line.size()>2&&line.substr(line.size()-2)=="--") { string v=trim(line.substr(0,line.size()-2)); if(env.count(v))env[v]=Value(env[v].toNumber()-1); return; }

    // compound assignment: +=, -=, *=, /=, %=
    for (auto& op : vector<string>{"+=","-=","*=","/=","%="}) {
        size_t pos = line.find(op);
        if (pos!=string::npos&&pos>0) {
            string lhs=trim(line.substr(0,pos));
            bool simple=true; for(char c:lhs)if(!isalnum(c)&&c!='_')simple=false;
            if (!simple) continue;
            Value rval=evaluate(trim(line.substr(pos+2)),env);
            if (op=="+="){
                Value l=env[lhs];
                if(l.type==Value::STRING||rval.type==Value::STRING) env[lhs]=Value(l.toString()+rval.toString());
                else env[lhs]=Value(l.toNumber()+rval.toNumber());
            }
            else if(op=="-=") env[lhs]=Value(env[lhs].toNumber()-rval.toNumber());
            else if(op=="*=") env[lhs]=Value(env[lhs].toNumber()*rval.toNumber());
            else if(op=="/=") env[lhs]=Value(env[lhs].toNumber()/rval.toNumber());
            else if(op=="%=") env[lhs]=Value(fmod(env[lhs].toNumber(),rval.toNumber()));
            return;
        }
    }

    // plain assignment: x = expr (but not ==)
    {
        size_t pos = line.find('=');
        if (pos!=string::npos&&pos>0&&line[pos-1]!='!'&&line[pos-1]!='<'&&line[pos-1]!='>'&&line[pos-1]!='='&&(pos+1>=(int)line.size()||line[pos+1]!='=')) {
            string lhs=trim(line.substr(0,pos));
            bool simple=true; for(char c:lhs)if(!isalnum(c)&&c!='_'&&c!='.')simple=false;
            if (simple&&!lhs.empty()) { env[lhs]=evaluate(trim(line.substr(pos+1)),env); return; }
        }
    }

    // any expression (console.log, function calls, etc.)
    evaluate(line, env);
}

// ═══════════════════════════════════════════════════════════
//  MULTI-LINE BLOCK EXECUTOR
// ═══════════════════════════════════════════════════════════
void executeLines(const vector<string>& lines, Env& env) {
    int i = 0;
    while (i < (int)lines.size()) {
        string line = trim(lines[i]);

        // ── function declaration ──────────────────────────
        if (line.size()>=8 && line.substr(0,8)=="function") {
            size_t lp=line.find('('), rp=line.find(')');
            string fname=trim(line.substr(8,lp-8));
            string paramStr=line.substr(lp+1,rp-lp-1);
            Function fn;
            for(auto& p:splitByComma(paramStr)) if(!trim(p).empty()) fn.params.push_back(trim(p));
            // handle inline statements after { on same line as function declaration
            size_t bracePos = line.find('{');
            if(bracePos!=string::npos) {
                string inline_rest = trim(line.substr(bracePos+1));
                if(!inline_rest.empty()) {
                    // split by ; and add each as body line
                    stringstream ss(inline_rest);
                    string token;
                    while(getline(ss, token, ';'))
                        if(!trim(token).empty()) fn.body.push_back(trim(token));
                }
            }
            i++;
            if(i<(int)lines.size()&&trim(lines[i])=="{") i++;
            int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                for(char c:bl){if(c=='{')depth++;else if(c=='}')depth--;}
                if(depth>0){
                    fn.body.push_back(lines[i]);
                } else if(depth==0){
                    string before=bl.substr(0, bl.rfind('}'));
                    if(!trim(before).empty()) fn.body.push_back(trim(before));
                }
                i++;
            }
            functions[fname]=fn;
            continue;
        
}

        // ── for loop ──────────────────────────────────────
       // ── for loop ──────────────────────────────────────
        if (line.size()>=3&&line.substr(0,3)=="for") {
            size_t lp=line.find('('), rp=line.rfind(')');
            string header=line.substr(lp+1,rp-lp-1);
            vector<string> parts; string cur; int d=0;
            for(char c:header){
                if(c=='('||c=='[')d++;else if(c==')'||c==']')d--;
                if(c==';'&&d==0){parts.push_back(trim(cur));cur="";}else cur+=c;
            }
            parts.push_back(trim(cur));
            i++;
            // skip standalone {
            if(i<(int)lines.size()&&trim(lines[i])=="{") i++;
            vector<string> body; int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                for(char c:bl){if(c=='{')depth++;else if(c=='}')depth--;}
                if(depth>0){
                    body.push_back(lines[i]);
                } else if(depth==0){
                    string before=bl.substr(0, bl.rfind('}'));
                    if(!trim(before).empty()) body.push_back(trim(before));
                }
                i++;
            }
            Env loopEnv=env;
            executeLine(parts[0],loopEnv);
            while(evaluate(parts[1],loopEnv).toBool()){
                Env iterEnv=loopEnv;
                try { executeLines(body,iterEnv); } catch(ReturnException& r){ throw; }
                for(auto& kv:iterEnv) loopEnv[kv.first]=kv.second;
                executeLine(parts[2],loopEnv);
            }
            for(auto& kv:loopEnv) if(env.count(kv.first)) env[kv.first]=kv.second;
            continue;
        }

        // ── while loop ────────────────────────────────────
        if (line.size()>=5&&line.substr(0,5)=="while") {
            size_t lp=line.find('('), rp=line.rfind(')');
            string cond=line.substr(lp+1,rp-lp-1);
            i++;
            vector<string> body;
            if(i<(int)lines.size()&&trim(lines[i])=="{") i++;
            int depth=1;
            while(i<(int)lines.size()&&depth>0){
                string bl=trim(lines[i]);
                for(char c:bl){if(c=='{')depth++;else if(c=='}')depth--;}
                if(depth>0){
                    body.push_back(lines[i]);
                } else if(depth==0){
                    string before=bl.substr(0, bl.rfind('}'));
                    if(!trim(before).empty()) body.push_back(trim(before));
                }
                i++;
            }
            while(evaluate(cond,env).toBool()){
                Env iterEnv=env;
                try { executeLines(body,iterEnv); } catch(ReturnException& r){ throw; }
                for(auto& kv:iterEnv) env[kv.first]=kv.second;
            }
            continue;
        }

        // ── if / else if / else ───────────────────────────
        if (line.size()>=2&&line.substr(0,2)=="if") {
            // collect all branches: { cond, body } pairs + optional else body
            struct Branch { string cond; vector<string> body; };
            vector<Branch> branches;
            vector<string> elseBody;
            bool hasElse = false;

            while (i < (int)lines.size()) {
                string bl = trim(lines[i]);
                if (bl.substr(0,7)=="else if"||bl.substr(0,7)=="} else ") {
                    // handled below
                }
                // parse condition
                size_t lp=bl.find('('), rp=bl.rfind(')');
                string cond = bl.substr(lp+1,rp-lp-1);
                i++;
                vector<string> body; int depth=1;
                while(i<(int)lines.size()&&depth>0){
                    string inner=trim(lines[i]);
                    for(char c:inner){if(c=='{')depth++;else if(c=='}')depth--;}
                    if(depth>0) body.push_back(lines[i]);
                    i++;
                }
                branches.push_back({cond,body});
                // peek at next line
                if (i<(int)lines.size()) {
                    string next=trim(lines[i]);
                    // normalize "} else" to "else"
                    if(next.substr(0,1)=="}") next=trim(next.substr(1));
                    if (next.substr(0,7)=="else if") { i++; /* re-loop with else if line */ 
                        // push fake "if" line so loop re-parses
                        // Actually just parse inline:
                        size_t lp2=next.find('('), rp2=next.rfind(')');
                        string cond2=next.substr(lp2+1,rp2-lp2-1);
                        i++;
                        vector<string> body2; int d2=1;
                        while(i<(int)lines.size()&&d2>0){
                            string inner=trim(lines[i]);
                            for(char c:inner){if(c=='{')d2++;else if(c=='}')d2--;}
                            if(d2>0) body2.push_back(lines[i]);
                            i++;
                        }
                        branches.push_back({cond2,body2});
                        // peek again
                        if(i<(int)lines.size()&&trim(lines[i]).substr(0,4)=="else"&&trim(lines[i]).find("if")==string::npos) {
                            i++;
                            int d3=1;
                            while(i<(int)lines.size()&&d3>0){
                                string inner=trim(lines[i]);
                                for(char c:inner){if(c=='{')d3++;else if(c=='}')d3--;}
                                if(d3>0) elseBody.push_back(lines[i]);
                                i++;
                            }
                            hasElse=true;
                        }
                        break;
                    } else if (next.substr(0,4)=="else"||next.substr(0,6)=="} else") {
                        i++;
                        int d3=1;
                        while(i<(int)lines.size()&&d3>0){
                            string inner=trim(lines[i]);
                            for(char c:inner){if(c=='{')d3++;else if(c=='}')d3--;}
                            if(d3>0) elseBody.push_back(lines[i]);
                            i++;
                        }
                        hasElse=true; break;
                    }
                }
                break;
            }

            bool executed=false;
            for(auto& br:branches){
                if(!executed && evaluate(br.cond,env).toBool()){
                    executeLines(br.body,env); executed=true;
                }
            }
            if(!executed&&hasElse) executeLines(elseBody,env);
            continue;
        }

        executeLine(lines[i], env);
        i++;
    }
}

// ═══════════════════════════════════════════════════════════
//  MAIN
// ═══════════════════════════════════════════════════════════
int main() {
    cout << "Enter JavaScript (type END on a new line to run):\n";
    vector<string> lines;
    string line;
    while (getline(cin, line)) {
        if (trim(line)=="END") break;
        lines.push_back(line);
    }
    Env env;
    try { executeLines(lines, env); }
    catch (ReturnException&) {}
    return 0;
}


