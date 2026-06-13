#include <iostream>
#include <string>
#include <map>
#include <regex>
#include <sstream>

using namespace std;

map<string, string> variables;

string resolveValue(const string& val) {
    string v = val;
    
    // Trim whitespace
    while (!v.empty() && isspace(v.front())) v.erase(v.begin());
    while (!v.empty() && isspace(v.back())) v.pop_back();

    // String literal (single or double quotes)
    if ((v.front() == '\'' && v.back() == '\'') ||
        (v.front() == '"'  && v.back() == '"')) {
        return v.substr(1, v.size() - 2);
    }

    // Variable reference
    if (variables.count(v)) return variables[v];

    return v; // Return as-is (number, etc.)
}

void processLine(const string& line) {
    string trimmed = line;
    while (!trimmed.empty() && isspace(trimmed.front())) trimmed.erase(trimmed.begin());
    while (!trimmed.empty() && isspace(trimmed.back())) trimmed.pop_back();

    // Remove trailing semicolon
    if (!trimmed.empty() && trimmed.back() == ';') trimmed.pop_back();

    // Match: let/const/var x = value
    regex varDecl(R"((let|const|var)\s+(\w+)\s*=\s*(.+))");
    smatch m;
    if (regex_match(trimmed, m, varDecl)) {
        variables[m[2]] = resolveValue(m[3]);
        return;
    }

    // Match: x = value (reassignment)
    regex reassign(R"((\w+)\s*=\s*(.+))");
    if (regex_match(trimmed, m, reassign)) {
        variables[m[1]] = resolveValue(m[2]);
        return;
    }

    // Match: console.log(...)
    regex consoleLog(R"(console\.log\((.+)\))");
    if (regex_match(trimmed, m, consoleLog)) {
        cout << resolveValue(m[1]) << endl;
        return;
    }
}

int main() {
    cout << "Enter JavaScript (type END to run):" << endl;

    string code, line;
    while (getline(cin, line)) {
        if (line == "END") break;
        code += line + "\n";
    }

    // Split by semicolon or newline and process each statement
    stringstream ss(code);
    while (getline(ss, line, '\n')) {
        // Handle multiple statements on one line (split by ;)
        stringstream ls(line);
        string stmt;
        while (getline(ls, stmt, ';')) {
            if (!stmt.empty()) processLine(stmt);
        }
    }

    return 0;
}