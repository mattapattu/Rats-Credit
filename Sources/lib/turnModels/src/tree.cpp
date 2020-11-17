#include "tree.h"

std::shared_ptr<TreeNode> newNode(std::string turn) 
{ 
    auto temp = std::make_shared<TreeNode>();
    temp->turn = turn; 
    return temp; 
} 

bool isChild(std::shared_ptr<TreeNode> currentNode, std::string newNode){
    bool foundChild = false;
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++) {
       if(i->turn == newNode){
           foundChild = true;
           break;
       }
    }
    return(foundChild);
}

std::shared_ptr<TreeNode> getChildNode(std::shared_ptr<TreeNode> currentNode, std::string newNode){
    for (auto i = currentNode->child.begin(); i != currentNode->child.end(); i++) {
       if(i->turn == newNode){
          break;
       }
    }
    return(i);
}

std::vector<double> getSiblingCredits(std::shared_ptr<TreeNode> currentNode){
    std::vector<double> credits;
    for (auto i = currentNode->siblings.begin(); i != currentNode->siblings.end(); i++) {
       credits.push_back(i->credit);
    }
    return(credits);
}

std::shared_ptr<TreeNode> initS1()
{
    auto root = newNode("E");
    auto dcb = newNode("dcb");
    auto dch = newNode("dch");
    auto gak = newNode("gak");
    auto gab = newNode("gab");
    auto bak = newNode("bak");    
    auto bag = newNode("bag");
    auto bcd = newNode("bcd");
    auto bch = newNode("bch");

      
    (root->child).push_back(dcb); 
    (root->child).push_back(dch); 
    (root->child).push_back(gak); 
    (root->child).push_back(gab); 
    (root->child[0]->child).push_back(bak); 
    (root->child[0]->child).push_back(bag); 
    (root->child[3]->child).push_back(bcd); 
    (root->child[3]->child).push_back(bch);  

    dcb->siblings.assign(dch,gak,gab);
    dch->siblings.assign(dcb,gak,gab);
    gak->siblings.assign(dch,dcb,gab);
    gab->siblings.assign(dch,gak,dcb);

    bak->siblings.assign(bag);
    bag->siblings.assign(bak);

    bcd->siblings.assign(bch);
    bch->siblings.assign(bcd);

    return(root);

}

std::shared_ptr<TreeNode> initS2()
{

    auto root = newNode("I");
    auto dcb = newNode("hcb");
    auto dch = newNode("hcd");
    auto gak = newNode("kag");
    auto gab = newNode("kab");
    auto bak = newNode("bak");    
    auto bag = newNode("bag");
    auto bcd = newNode("bcd");
    auto bch = newNode("bch");   

    (root->child).push_back(hcb); 
    (root->child).push_back(hcd); 
    (root->child).push_back(kag); 
    (root->child).push_back(kab); 
   
    (root->child[0]->child).push_back(bak); 
    (root->child[0]->child).push_back(bag); 
    (root->child[3]->child).push_back(bcd); 
    (root->child[3]->child).push_back(bch);  

    hcb->siblings.assign(hcd,kag,kab);
    hcd->siblings.assign(hcb,kag,kab);
    kag->siblings.assign(hcb,hcd,kab);
    kab->siblings.assign(hcb,hcd,kag);

    bak->siblings.assign(bag);
    bag->siblings.assign(bak);

    bcd->siblings.assign(bch);
    bch->siblings.assign(bcd);

    return(root);

}
