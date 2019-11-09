#include <iostream>
#include <vector>
#include <set>
#include <functional>
#include <numeric>

int intBreak(int n) {
	//std::cout<<"Updated algorithms" <<std::endl;
	std::vector<int > vec;
	
	vec.push_back(1); // for 0
	vec.push_back(1); // for 1

	
	vec.push_back(1);

	

	for (int j = 3 ; j<=n ; ++j) {
		auto maxProduct = 1;
		if (j % 2 == 0) {
			maxProduct = (j/2)*(j/2);
		}else {
			maxProduct = (j/2)*((j/2) + 1);
		}


		for (int k = 2; k < j; ++k) {
			auto pending = j-k;
			auto prod = pending*vec[k];
			if (prod > maxProduct) {
				maxProduct = prod;
			}
		} 

		vec.push_back(maxProduct);


	}

 //    auto count = 0;
	// for (auto v : vec) {
	// 	std::cout<<"Max product for " << count << " is " << v <<std::endl;
	// 	count++;
	// }
	return vec[n];

}
int integerBreak(int n) {
	std::vector<std::set<std::multiset<int> > > vec;
	std::multiset<int> temp;
	std::set<std::multiset<int> > s;
	s.insert(temp);
	vec.push_back(s); //0
	vec.push_back(s); //1

    std::multiset<int> second;
    second.insert(1);
    second.insert(1);
    std::set<std::multiset<int>> secondSet;
    secondSet.insert(second); 


    vec.push_back(secondSet); //2

    auto maxProduct = 1;

    for (int j =  3; j <= n; ++j) {
    	std::set<std::multiset<int> > s;
    	for (int k = 2 ; k < j ; ++k) {
    		auto interimSet(vec[k]);
    		for (auto itr : interimSet) {
    			auto itrCopy(itr);
    			itrCopy.insert(j-k);

    			auto multi = std::accumulate(std::begin(itrCopy),std::end(itrCopy),1,std::multiplies<int>());
    			if (multi > maxProduct) {
    				maxProduct = multi;	
    			}

    			s.insert(itrCopy);
    		}
    		
    		
    	}

    	for (int k = 1 ; k < j ; ++k) {
    		std::multiset<int> interimSet;
    		interimSet.insert(k);
    		interimSet.insert(j-k);

    		auto multi = std::accumulate(std::begin(interimSet),std::end(interimSet),1,std::multiplies<int>());
    		if (multi > maxProduct) {
    			maxProduct = multi;	
    		}
    		s.insert(interimSet);
    	}

    	vec.push_back(s);
    }

    auto count = 0;
    for (auto itr : vec) {
    	auto maxMulti = 1;
    	for (auto itr1 : itr) {
    		auto multi = 1;
    		for (auto itr2 : itr1) {
    			std::cout<<itr2<<",";
    			multi = multi * itr2;
    		}
    		std::cout<<"||";
    		if (multi > maxMulti) {
    			 maxMulti = multi;
    		}
    	}

        std::cout <<" Max Product for " << count << " is " << maxMulti;
        count++;
    	std::cout<<std::endl;
    }
    // auto finalMultiSet = vec[n];
    // //auto maxProduct = 1;

    // for (auto interimMultiSet : finalMultiSet) {
    // 	auto multi = std::accumulate(std::begin(interimMultiSet),std::end(interimMultiSet),1,std::multiplies<int>());
    // 	if (multi > maxProduct) {
    // 		maxProduct = multi;
    // 	}
    // }

    return maxProduct;
    //return 0;
    

}

int main() {
	std::cout<<integerBreak(10)<<std::endl;
	std::cout<<intBreak(10)<<std::endl;
}