#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;

string board[16][16];

void print_board(string in_str){
    ifstream ifile(in_str);

    for(int i=0; i<16; i++)
        for(int j=0; j<16; j++)
            board[i][j] = " ";

    string str;
    int count = 0;
    while(ifile >> str){
        count += 1;
        int st = str.find("(", 0);

        string type = "";
        type = type.append(str, 0, st);

        int x = 0;
        while(str[++st] != ','){
            x *= 10 ;
            x += str[st] - 48 ;
        }

        int y = 0;
        while(str[++st] != ')'){
            y *= 10 ;
            y += str[st] - 48 ;
        }

        if(type == "black")
            board[x][y] = "B";
        else
            board[x][y] = "W";
    }

    if(count%2 == 0)
        cout << "Next Black" << endl;
    else 
        cout << "Next White" << endl;

    for(int i=0; i<16; i++) cout << setw(3) << i << "|";
    cout << endl;
    for(int i=1; i<=15; i++){
        cout << setw(3) << i << "|";
        for(int j=1; j<=15; j++){
            cout << setw(3) << board[j][i] << "|";
        }
        cout << endl << "--------------------------------------------------------------"<< endl;
    }
}

void print_score_board(string in_str){
    ifstream ifile(in_str);

    for(int i=0; i<16; i++) cout << setw(8) << i;
    cout << endl;

    int board_score[16][16] = {0}, x, y, score;
    while(ifile >> x >> y >> score){
        board_score[x][y] = score;
    }

    for(int i=1; i<=15; i++){
        cout << setw(7) << i ;
        for(int j=1; j<=15; j++){
            if(board[j][i] == " ")
                cout << setw(8) << board_score[j][i];
            else
                cout << setw(8) << board[j][i];
        }
        cout << endl;
    }
}

int main(){

    print_board("test.in");

    print_score_board("test.out");

}