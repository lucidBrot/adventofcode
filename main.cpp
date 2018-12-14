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
const Cart InverseCart[] = {Cart::None, Cart::Up, Cart::Down, Cart::Left, Cart::Right, Cart::Crashed};

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
            if (cart != Cart::None){
                cartStorage.coeffRef(y,x) = static_cast<int>(cart);
            }
            x++;
        }
        x=0;
        y++;
    }
    assert( maxY == y);
    return cartStorage;

}

enum class Track : int {None=0, Slash, Backslash, Minus, Plus, Pipe};
const Track InverseTrack[] = {Track::None, Track::Slash, Track::Backslash, Track::Minus, Track::Plus, Track::Pipe};

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

            if(track != Track::None){
                trackStorage.coeffRef(y,x) = static_cast<int>(track);
            }
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

struct TrackWithPos {
    Track track;
    long unsigned int x;
    long unsigned int y;
};

struct CartWithPos {
    Cart cart;
    long unsigned int x;
    long unsigned int y;
};

struct TrackWithPos getNextTrackForCart(SMatrix trackData, size_t cartX, size_t cartY, Cart cartOrientation){
    size_t x=0, y=0;
    switch(cartOrientation){
        case Cart::Up:
            y=-1;
            break;
        case Cart::Down:
            y=1;
            break;
        case Cart::Left:
            x=-1;
            break;
        case Cart::Right:
            x=1;
            break;
        case Cart::Crashed:
            assert(0 && "A crashed cart has no next destination!");
            break;
        case Cart::None:
            assert(0 && "The cart isn't real");
            break;
    }

    size_t nextX = cartX+x;
    size_t nextY = cartY+y;

    int trackNum = trackData.coeffRef(cartY, cartX);
    struct TrackWithPos twp = {InverseTrack[trackNum], // track
                                nextX, // x
                                nextY // y
                                };
    return twp;

}

// function :: cart coordinates, orientation, fact that next is an intersection --> which orientation is next
Cart getOrientationAfterIntersection(Cart originalOrientation){
    assert(originalOrientation != Cart::None && originalOrientation != Cart::Crashed);
    
    // TODO: store, and get, carts latest decision
    enum class Decision : int {left, straight, right};
    Decision decision = Decision::left;
    std::cout << "TODO: don't hardcode decision!" << std::endl;

    if(decision==Decision::straight){
        return originalOrientation;
    }

    // compute assuming decision==left
    Cart leftDecision;
    // We know that cart drives in direction of its orientation. So there lies the intersection.
    switch(originalOrientation){
        case Cart::Up:
            leftDecision = Cart::Left;
            break;
        case Cart::Down:
            leftDecision = Cart::Right;
            break;
        case Cart::Left:
            leftDecision = Cart::Down;
            break;
        case Cart::Right:
            leftDecision = Cart::Up;
            break;
        default: // TODO: do default case, and make sure to invert leftdecision i decision == right
            assert(0 && "Broken Orientation parameter!");
            break;
    }

    if(decision==Decision::left){
        return leftDecision;
    }

    auto invertDecision = [](Cart decision){
        switch(decision){
            case Cart::Up:
                return Cart::Down;
            case Cart::Down:
                return Cart::Up;
            case Cart::Left:
                return Cart::Right;
            case Cart::Right:
                return Cart::Left;
            default:
                assert(0);
        }
    };

    //: if(decision==Decision::right){
    return invertDecision(leftDecision);
}


struct CartWithPos getNextCart(struct CartWithPos originalCart, struct TrackWithPos nextTrack){
    assert(originalCart.cart!=Cart::Crashed); assert(originalCart.cart!=Cart::None);
    assert(nextTrack.track != Track::None);
    struct CartWithPos nextCart;
    nextCart.x = nextTrack.x;
    nextCart.y = nextTrack.y;

    // TODO: handle intersections

    switch(originalCart.cart){
        case Cart::Up:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Minus:
                        assert(0 && "This should not happen: Moving upwards onto horizontal track!");
                        break;
                    case Track::Pipe:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::None:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Down:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Minus:
                        assert(0 && "This should not happen: Moving down onto horizontal track!");
                        break;
                    case Track::Pipe:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::None:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Right:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::Minus:
                        nextCart.cart = Cart::Right;
                        break;
                    case Track::Pipe:
                        assert(0 && "Bad things are happening: Moving horizontally onto a pipe!");
                        break;
                    case Track::None:
                        assert(0);
                        break;

                }
            }
            break;
        case Cart::Left:
            {
                switch(nextTrack.track){
                    case Track::Slash:
                        nextCart.cart = Cart::Down;
                        break;
                    case Track::Backslash:
                        nextCart.cart = Cart::Up;
                        break;
                    case Track::Minus:
                        nextCart.cart = Cart::Left;
                        break;
                    case Track::Pipe:
                        assert(0 && "Bad things are happening: Moving horizontallii onto a pipe!");
                        break;
                    case Track::None:
                        assert(0);
                        break;

                }
            }
            break;
        // these won't happen. Let the compiler know that it does not need warnings.
        case Cart::Crashed:
            [[fallthrough]];
        case Cart::None:
            assert(0);
    }

    return nextCart;
}

void moveCarts(SMatrix carts, SMatrix trackData){

    // get nonzero entries - since zero stands for None
    std::cout << "Carts: ";
    for (int k=0; k<carts.outerSize(); ++k)
        for (SMatrix::InnerIterator it(carts,k); it; ++it)
        {
            it.row();   // row index
            it.col();   // col index (here it is equal to k)
            it.index(); // inner index, here it is equal to it.row()
            std::cout << "(" << it.row() << ", " << it.col() << ")\t";

            // find the next track to be driven on
            Cart cart = InverseCart[it.value()];
            struct TrackWithPos nextTrack = getNextTrackForCart(trackData, it.row(), it.col(), cart);
            struct CartWithPos originalCart = {
                cart, // cart
                (long unsigned int) it.row(), // x
                (long unsigned int) it.col(), // y
            };

            // figure out where to go next
            struct CartWithPos nextCart = getNextCart(originalCart, nextTrack);

            // TODO: update carts matrix
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

    // TODO: replace carts in input SMatrix with straight tracks
    // TODO: another SMatrix which contains info about the next intersection turn

    moveCarts(carts, tracks);

}
