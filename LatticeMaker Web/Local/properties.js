var getAggrFromTextArea = function(idTextarea, idAggrCombo1, idAggrCombo2)
{    
    var fill_connectives = function()
    {
        var regex_and = /\s*and_([^\s()]+)(\(.+\))?\s*:-/g;
        var regex_or = /\s*or_([^\s()]+)(\(.+\))?\s*:-/g;
        var regex_aggr = /\s*aggr_([^\s()]+)(\(.+\))?\s*:-/g;
        
        $('#'+idAggrCombo1).empty();
        $('#'+idAggrCombo2).empty();
        addConnective(regex_and,'&');
        addConnective(regex_or,'|');
        addConnective(regex_aggr,'@');
        
        $('#'+idAggrCombo1+' option').clone().appendTo('#'+idAggrCombo2);
        
    }
    
    var addConnective = function(regex,symbol)
    {   
        var regex_con = regex;
        var match = true;
        
        while (match = regex_con.exec(textarea.value)) 
        { 
            var connective = document.createElement("option");
            
            var regex_arity = /[^,()]+/g;
            var arity = 0;
            
            while (match[2] != undefined && regex_arity.exec(match[2])) arity++;
            
            connective.text = symbol+'_'+match[1]+'/'+arity.toString();
            
            combo1.add(connective); 
        }
    }
    
    var textarea = document.getElementById(idTextarea);
    var combo1 = document.getElementById(idAggrCombo1);
    var combo2 = document.getElementById(idAggrCombo2);
    
    jQuery('#'+idTextarea).bind('change',fill_connectives);
    
    $('#'+idTextarea).trigger('change');
}