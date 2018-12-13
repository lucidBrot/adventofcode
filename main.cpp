#include <iostream>
#include <Eigen/Dense>
using Eigen::MatrixXd;
#include <sstream>
#include <string>
#include <cassert>

enum class Cart {None=0, Up, Down, Left, Right, Crashed};


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

int main() { 
    std::cout << std::endl;
  MatrixXd m(2,2); 
  m(0,0) = 3; 
  m(1,0) = 2.5; 
  m(0,1) = -1; 
  m(1,1) = m(1,0) + m(0,1); 
  std::cout << m << std::endl; 

  std::cout << parseCartsPositions("asdv\nfgv>", 4, 2) << std::endl;
}
