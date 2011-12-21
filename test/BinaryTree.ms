var minDepth = 4;

function bottomUpTree(item, depth) {
    if (depth > 0) then 
        return {
            0:item,
            1:bottomUpTree((2 * item) - 1, depth - 1),
            2:bottomUpTree(2 * item, depth - 1),
            3:false
        };
    else
        return {0:item, 3:true};
}
    
function itemCheck(this) {
    if (this[3]) then return this[0];
    else return this[0] + itemCheck(this[1]) - itemCheck(this[2]);
}

function pow2(n) {
    var p = 1;
    while (n > 0) do {
        p = p * 2;
        n = n - 1;
    }
    return p;
}

procedure main() {
    var n = 17;
    
    var maxDepth;
    if (minDepth + 2 > n) then maxDepth = minDepth + 2;
    else maxDepth = n;
    var stretchDepth = maxDepth + 1;
    
    var check = itemCheck(bottomUpTree(0, stretchDepth));
    call print("stretch tree of depth " + stretchDepth + " check: " + check);
    
    var longLivedTree = bottomUpTree(0, maxDepth);
    
    var depth = minDepth;
    var iterations = pow2(maxDepth);
    while (depth <= maxDepth) do {
        check = 0;
        
        var i = 1;
        while (i <= iterations) do {
            check = check + itemCheck(bottomUpTree(i, depth));
            check = check + itemCheck(bottomUpTree(-i, depth));
            i = i + 1;
        }
        call print((iterations * 2) + " trees of depth " + depth + " check: " + check);
        depth = depth + 2;
        iterations = iterations / 4;
    }   
    call print("long lived tree of depth " + maxDepth + " check: "+ itemCheck(longLivedTree));
}
