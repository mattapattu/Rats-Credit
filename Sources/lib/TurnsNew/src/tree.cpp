#include <Rcpp.h>
#include "tree.hpp"

std::shared_ptr<TreeNode> newNode(std::string turn)
{
    auto temp = std::make_shared<TreeNode>();
    temp->turn = turn;
    temp->credit=0;
    //Rcpp::Rcout <<"Creating node ="<<turn<<std::endl;
    return temp;
}

bool isChild(std::shared_ptr<TreeNode> currentNode, std::string newNode)
{
    bool foundChild = false;
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++)
    {
        if ((*i)->turn == newNode)
        {
            foundChild = true;
            break;
        }
    }
    return (foundChild);
}

std::shared_ptr<TreeNode> getChildNode(std::shared_ptr<TreeNode> currentNode, std::string newNode)
{
    //Rcpp::Rcout <<"Getting child nodes of currentNode= "<<currentNode->turn<<std::endl;
    std::shared_ptr<TreeNode> child;
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++)
    {
        //Rcpp::Rcout <<"child node = "<< (*i)->turn<<std::endl;
        if ((*i)->turn == newNode)
        {
            child = *i;
            break;
        }
    }
        
    return (child);
}


std::vector<double> getChildrenCredits(std::shared_ptr<TreeNode> parentNode)
{
    std::vector<double> credits;
    for (auto i = parentNode->child.begin(); i != parentNode->child.end(); i++)
    {
        std::shared_ptr<TreeNode> stgPtr = (*i);
        credits.push_back(stgPtr->credit);
    }
    return (credits);
}

std::shared_ptr<TreeNode> initS1()
{
    std::shared_ptr<TreeNode> root = newNode("E");
    std::shared_ptr<TreeNode> dc1 = newNode("dc1");
    std::shared_ptr<TreeNode> fga1 = newNode("fga1");
    std::shared_ptr<TreeNode> c2h = newNode("c2h");
    std::shared_ptr<TreeNode> c2ba1 = newNode("c2ba1");
    std::shared_ptr<TreeNode> a2bc1 = newNode("a2bc1");
    std::shared_ptr<TreeNode> a2kj = newNode("a2kj");
    std::shared_ptr<TreeNode> a2gf = newNode("a2gf");
    std::shared_ptr<TreeNode> c2d = newNode("c2d");

    (root->child).push_back(dc1);
    (root->child[0]->child).push_back(c2h);
    (root->child[0]->child).push_back(c2ba1);
    (root->child[0]->child[1]->child).push_back(a2kj);
    (root->child[0]->child[1]->child).push_back(a2gf);

    (root->child).push_back(fga1);
    (root->child[1]->child).push_back(a2bc1);
    (root->child[1]->child).push_back(a2kj);
    (root->child[1]->child[0]->child).push_back(c2d);
    (root->child[1]->child[0]->child).push_back(c2h);
    

    return (root);
}

std::shared_ptr<TreeNode> initS2()
{

    std::shared_ptr<TreeNode> root = newNode("I");
    std::shared_ptr<TreeNode> hc1 = newNode("hc1");
    std::shared_ptr<TreeNode> jka1 = newNode("jka1");
    std::shared_ptr<TreeNode> c2d = newNode("c2d");
    std::shared_ptr<TreeNode> c2ba1 = newNode("c2ba1");
    std::shared_ptr<TreeNode> a2bc1 = newNode("a2bc1");
    std::shared_ptr<TreeNode> a2gf = newNode("a2gf");
    std::shared_ptr<TreeNode> a2kj = newNode("a2kj");
    std::shared_ptr<TreeNode> c2h = newNode("c2h");

    (root->child).push_back(hc1);
    (root->child[0]->child).push_back(c2d);
    (root->child[0]->child).push_back(c2ba1);
    (root->child[0]->child[1]->child).push_back(a2kj);
    (root->child[0]->child[1]->child).push_back(a2gf);

    (root->child).push_back(jka1);
    (root->child[1]->child).push_back(a2bc1);
    (root->child[1]->child).push_back(a2gf);
    (root->child[1]->child[0]->child).push_back(c2d);
    (root->child[1]->child[0]->child).push_back(c2h);

    return (root);
}
