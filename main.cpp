#include <iostream>
#include <Eigen/Dense>
#include <Eigen/Sparse>
using Eigen::MatrixXd;
typedef Eigen::SparseMatrix<int, Eigen::RowMajor> SMatrix;
#include <sstream>
#include <string>
#include <cassert>
#include <tuple>
#include <fstream>
#include <streambuf>

enum class Cart : int {None=0, Up, Down, Left, Right, Crashed};

SMatrix parseCartsPositions(std::string filecontents, unsigned int maxX, unsigned int maxY){
    SMatrix cartStorage; cartStorage.resize(maxY, maxX); cartStorage.reserve(20);
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
            cartStorage.coeffRef(y,x) = static_cast<int>(cart);
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return cartStorage;

}

enum class Track : int {None=0, Slash, Backslash, Minus, Plus, Pipe};

SMatrix parseTracks(std::string filecontents, unsigned int maxX, unsigned int maxY){

    SMatrix trackStorage; trackStorage.resize(maxY, maxX); trackStorage.reserve(20);
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
            trackStorage.coeffRef(y,x) = static_cast<int>(track);
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

void moveCarts(SMatrix carts, SMatrix data){

    for (int k=0; k<carts.outerSize(); ++k)
        for (SMatrix::InnerIterator it(carts,k); it; ++it)
        {
            it.value();
            it.row();   // row index
            it.col();   // col index (here it is equal to k)
            it.index(); // inner index, here it is equal to it.row()
            std::cout << "(" << it.row() << ", " << it.col() << ")\t";
        }
    std::cout << std::endl;
}


int main() { 
    std::cout << std::endl << "carts:" << std::endl;
    std::string input = readFileToString("input1.txt");
    SMatrix carts = parseCartsPositions(input, 13, 6); 
    std::cout << carts << std::endl << std::endl << "tracks:" << std::endl;
    SMatrix tracks = parseTracks(input, 13, 6);
    std::cout << tracks << std::endl;

    moveCarts(carts, tracks);

}
