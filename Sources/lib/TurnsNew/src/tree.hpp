#ifndef __TREE__
#define __TREE__

#include <iostream>
#include <vector>
#include <memory>

struct TreeNode
{
    // member vars
    std::string turn;
    double credit;
    std::vector<std::shared_ptr<TreeNode>> child; 
    
};

std::shared_ptr<TreeNode> newNode(std::string turn);
std::shared_ptr<TreeNode> initS1();
std::shared_ptr<TreeNode> initS2();
bool isChild(std::shared_ptr<TreeNode> currentNode, std::string newNode);
std::shared_ptr<TreeNode> getChildNode(std::shared_ptr<TreeNode> currentNode, std::string newNode);
std::vector<double> getSiblingCredits(std::shared_ptr<TreeNode> currentNode);
std::vector<double> getChildrenCredits(std::shared_ptr<TreeNode> parentNode);


#endif