; Utility functions for windgram generation

quote = inttochar(34) ; WTF is wrong with NCL

undef("replaceLabelInString")
function replaceLabelInString(original_string, label, replacement)
begin
 return(systemfunc("echo " + original_string + " | sed s/" + label + "/" + replacement + "/g"))
end
