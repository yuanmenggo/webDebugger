// JavaScript Document
var Common = (function() {
  var contains = function(array, v) 
  {
      var i = array.length;
      while (i>=0) 
      {
	     	i--;
        if (array[i] === v) 
        {
            return i;
        }
      }
      return -1;
  }
  
  return 
  {
	contains:contains	  
  }
})();