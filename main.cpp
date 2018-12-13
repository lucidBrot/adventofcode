#include <iostream>
#include <Eigen/Dense>
using Eigen::MatrixXd;
#include <sstream>
#include <string>
#include <cassert>
#include <tuple>
#include <fstream>
#include <streambuf>

enum class Cart : int {None=0, Up, Down, Left, Right, Crashed};

MatrixXd parseCartsPositions(std::string filecontents, unsigned int maxX, unsigned int maxY){
    MatrixXd cartStorage = MatrixXd::Zero(maxY, maxX);
    std::istringstream f(filecontents);
    std::string line;
    unsigned int x = 0;
    unsigned int y = 0;
    while (std::getline(f, line)) {
        for (char& c : line){
            // read cart positions and nothing else
            Cart cart;
            switch(c){
                case 'v':
                    // down cart
                    cart = Cart::Down;
                    break;
                case '^':
                    cart = Cart::Up;
                    break;
                case '<':
                    cart = Cart::Left;
                    break;
                case '>':
                    cart = Cart::Right;
                    break;

                default:
                    // not a cart, but increases coordinate nonetheless
                    cart = Cart::None;
                    break;
            }
            cartStorage(y,x) = static_cast<int>(cart);
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return cartStorage;

}

enum class Track : int {Slash, Backslash, Minus, Plus, Pipe, None};

MatrixXd parseTracks(std::string filecontents, unsigned int maxX, unsigned int maxY){

    MatrixXd trackStorage = MatrixXd::Zero(maxY, maxX);
    std::istringstream f(filecontents);
    std::string line;
    unsigned int x = 0;
    unsigned int y = 0;
    while (std::getline(f, line)) {
        for (char& c : line){
            // read track positions and nothing else
            Track track;
            switch(c){
                case '/':
                    track = Track::Slash; break;
                case '\\':
                    track = Track::Backslash; break;
                case '-':
                    track = Track::Minus; break;
                case '+':
                    track = Track::Plus; break;
                case '|':
                    track = Track::Pipe; break;
                default:
                    track = Track::None; break;
            }
            trackStorage(y,x) = static_cast<int>(track);
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return trackStorage;
}

std::string readFileToString(std::string filename){
    std::ifstream t(filename);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}

int main() { 
    std::cout << std::endl;
    std::string input = readFileToString("input1.txt");
    MatrixXd carts = parseCartsPositions(input, 13, 6); 
    std::cout << carts << std::endl << std::endl;
    MatrixXd tracks = parseTracks(input, 13, 6);
    std::cout << tracks << std::endl;

}
