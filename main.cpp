#include <bits/stdc++.h>
using namespace std;

struct Term {
    long long a; // coefficient
    int b, c, d; // exponents for x, sin, cos
    bool operator==(const Term& o) const { return b==o.b && c==o.c && d==o.d; }
};

struct Poly {
    vector<Term> t;
    void simplify(){
        sort(t.begin(), t.end(), [](const Term& x, const Term& y){
            if(x.b!=y.b) return x.b>y.b;
            if(x.c!=y.c) return x.c>y.c;
            return x.d>y.d;
        });
        vector<Term> res;
        for(auto &term: t){
            if(term.a==0) continue;
            if(!res.empty() && res.back()==term){
                res.back().a += term.a;
            } else {
                res.push_back(term);
            }
        }
        t.clear();
        for(auto &term: res) if(term.a!=0) t.push_back(term);
    }
    Poly operator+(const Poly& o) const { Poly r; r.t = t; r.t.insert(r.t.end(), o.t.begin(), o.t.end()); r.simplify(); return r; }
    Poly operator-(const Poly& o) const { Poly r; r.t = t; for(auto &term: o.t){ Term nt = term; nt.a = -nt.a; r.t.push_back(nt);} r.simplify(); return r; }
    Poly operator*(const Poly& o) const { Poly r; r.t.reserve(t.size()*o.t.size()); for(const auto &a: t){ for(const auto &b: o.t){ Term nt; nt.a = a.a * b.a; nt.b = a.b + b.b; nt.c = a.c + b.c; nt.d = a.d + b.d; r.t.push_back(nt); } } r.simplify(); return r; }
    Poly derivative() const { Poly r; for(const auto &term: t){ if(term.b!=0){ Term nt = term; nt.a *= term.b; nt.b -= 1; r.t.push_back(nt); } if(term.c!=0){ Term nt = term; nt.a *= term.c; nt.c -= 1; nt.d += 1; r.t.push_back(nt); } if(term.d!=0){ Term nt = term; nt.a *= term.d; nt.d -= 1; nt.c += 1; nt.a = -nt.a; r.t.push_back(nt); } } r.simplify(); return r; }
    string toString() const {
        if(t.empty()) return "0";
        string res;
        for(size_t i=0;i<t.size();++i){ const Term &term = t[i]; long long coeff = term.a; if(i==0){ if(coeff<0){ res.push_back('-'); coeff = -coeff; } } else { if(coeff<0){ res.push_back('-'); coeff = -coeff; } else res.push_back('+'); }
            bool needCoeff = !(coeff==1 && (term.b!=0 || term.c!=0 || term.d!=0));
            if(needCoeff) res += to_string(coeff);
            if(term.b!=0){ if(term.b==1) res += "x"; else res += "x^" + to_string(term.b); }
            if(term.c!=0){ if(term.c==1) res += "sinx"; else res += "sin^" + to_string(term.c) + "x"; }
            if(term.d!=0){ if(term.d==1) res += "cosx"; else res += "cos^" + to_string(term.d) + "x"; }
        }
        return res;
    }
};

struct Frac {
    Poly p, q; // numerator, denominator
    Frac(){}
    Frac(const Poly& _p, const Poly& _q):p(_p),q(_q){ p.simplify(); q.simplify(); }
    static Poly onePoly(){ Poly one; Term t; t.a=1; t.b=0; t.c=0; t.d=0; one.t.push_back(t); one.simplify(); return one; }
    Frac operator+(const Frac& o) const { Poly num = p * o.q + o.p * q; Poly den = q * o.q; return Frac(num, den); }
    Frac operator-(const Frac& o) const { Poly num = p * o.q - o.p * q; Poly den = q * o.q; return Frac(num, den); }
    Frac operator*(const Frac& o) const { Poly num = p * o.p; Poly den = q * o.q; return Frac(num, den); }
    Frac operator/(const Frac& o) const { Poly num = p * o.q; Poly den = q * o.p; return Frac(num, den); }
    Frac derivative() const { Poly num = p.derivative() * q - p * q.derivative(); Poly den = q * q; return Frac(num, den); }
    string toString() const {
        Poly num = p; Poly den = q; num.simplify(); den.simplify();
        if(num.t.empty()) return "0";
        bool denIsOne = (den.t.size()==1 && den.t[0].a==1 && den.t[0].b==0 && den.t[0].c==0 && den.t[0].d==0);
        string sNum = num.toString();
        string sDen = den.toString();
        if(denIsOne) return sNum;
        if(num.t.size()>1) sNum = "(" + sNum + ")";
        if(den.t.size()>1) sDen = "(" + sDen + ")";
        return sNum + "/" + sDen;
    }
};

enum TokenType {TOK_END, TOK_PLUS, TOK_MINUS, TOK_MUL, TOK_DIV, TOK_LP, TOK_RP, TOK_NUM, TOK_X, TOK_SIN, TOK_COS, TOK_POW};
struct Token{TokenType type; long long value;};

string s; int pos; Token cur;

void skipSpace(){ while(pos<(int)s.size() && isspace(s[pos])) pos++; }
Token nextToken(){ skipSpace(); if(pos>= (int)s.size()) return {TOK_END,0}; char ch=s[pos];
    if(ch=='+'){pos++;return{TOK_PLUS,0};}
    if(ch=='-'){pos++;return{TOK_MINUS,0};}
    if(ch=='*'){pos++;return{TOK_MUL,0};}
    if(ch=='/'){pos++;return{TOK_DIV,0};}
    if(ch=='('){pos++;return{TOK_LP,0};}
    if(ch==')'){pos++;return{TOK_RP,0};}
    if(ch=='^'){pos++;return{TOK_POW,0};}
    if(isdigit(ch)){
        long long v=0; while(pos<(int)s.size() && isdigit(s[pos])){v=v*10+(s[pos]-'0');pos++;}
        return {TOK_NUM,v};
    }
    if(isalpha(ch)){
        if(s.compare(pos,3,"sin")==0){pos+=3;return{TOK_SIN,0};}
        if(s.compare(pos,3,"cos")==0){pos+=3;return{TOK_COS,0};}
        if(s[pos]=='x'){pos++;return{TOK_X,0};}
    }
    pos++; return nextToken();
}
void advance(){ cur = nextToken(); }

bool isPrimary(const Token& tk){ return tk.type==TOK_NUM||tk.type==TOK_X||tk.type==TOK_SIN||tk.type==TOK_COS||tk.type==TOK_LP; }

Frac parseExpr();
Frac parseTerm();
Frac parseFactor();

Frac parseExpr(){ Frac left=parseTerm(); while(cur.type==TOK_PLUS||cur.type==TOK_MINUS){ Token op=cur; advance(); Frac right=parseTerm(); if(op.type==TOK_PLUS) left=left+right; else left=left-right; } return left; }
Frac parseTerm(){ Frac left=parseFactor(); while(cur.type==TOK_MUL||cur.type==TOK_DIV){ Token op=cur; advance(); Frac right=parseFactor(); if(op.type==TOK_MUL) left=left*right; else left=left/right; } return left; }

Frac makeConstPoly(long long v){ Poly p; Term t; t.a=v; t.b=0; t.c=0; t.d=0; p.t.push_back(t); p.simplify(); return Frac(p, Frac::onePoly()); }
Frac makeVarPoly(int exp){ Poly p; Term t; t.a=1; t.b=exp; t.c=0; t.d=0; p.t.push_back(t); p.simplify(); return Frac(p, Frac::onePoly()); }
Frac makeSinPoly(int exp){ Poly p; Term t; t.a=1; t.b=0; t.c=exp; t.d=0; p.t.push_back(t); p.simplify(); return Frac(p, Frac::onePoly()); }
Frac makeCosPoly(int exp){ Poly p; Term t; t.a=1; t.b=0; t.c=0; t.d=exp; p.t.push_back(t); p.simplify(); return Frac(p, Frac::onePoly()); }

Frac parseFactor(){ bool neg=false; if(cur.type==TOK_PLUS){ advance(); } else if(cur.type==TOK_MINUS){ neg=true; advance(); }
    Frac result;
    if(cur.type==TOK_NUM){ long long v=cur.value; advance(); result=makeConstPoly(v); }
    else if(cur.type==TOK_X){ advance(); int exp=1; if(cur.type==TOK_POW){ advance(); if(cur.type==TOK_NUM){ exp=(int)cur.value; advance(); } } result=makeVarPoly(exp); }
    else if(cur.type==TOK_SIN){ advance(); int exp=1; if(cur.type==TOK_POW){ advance(); if(cur.type==TOK_NUM){ exp=(int)cur.value; advance(); } } if(cur.type==TOK_X) advance(); result=makeSinPoly(exp); }
    else if(cur.type==TOK_COS){ advance(); int exp=1; if(cur.type==TOK_POW){ advance(); if(cur.type==TOK_NUM){ exp=(int)cur.value; advance(); } } if(cur.type==TOK_X) advance(); result=makeCosPoly(exp); }
    else if(cur.type==TOK_LP){ advance(); result=parseExpr(); if(cur.type==TOK_RP) advance(); }
    else { result=makeConstPoly(0); }
    while(isPrimary(cur)){
        Frac nxt=parseFactor(); result=result*nxt; }
    if(neg){ Frac minus=makeConstPoly(-1); result=minus*result; }
    return result; }

int main(){ ios::sync_with_stdio(false); cin.tie(nullptr);
    string input; if(!(cin>>input)) return 0; s=input; pos=0; advance(); Frac expr=parseExpr(); expr.p.simplify(); expr.q.simplify(); cout<<expr.toString()<<"\n"; Frac der=expr.derivative(); der.p.simplify(); der.q.simplify(); cout<<der.toString()<<"\n"; return 0; }
