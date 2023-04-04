%% Function getting behavior log
function [beha_log]=behalog1(x)
beha_log = struct;
unit2=1;
unit3=1;
for q = 1:length(x.Behavior)
    if strcmp(x.Behavior{q}, 'Other vocalize') && ~strcmp(x.id{q}, 'multiple') && strcmp(x.Start_end{q},'start')
       beha_log.other_vocals{unit2} = 'Other vocalize (single)';
       beha_log.other_time{unit2} = x.Time{q};
       beha_log.caller_con{unit2} = x.c_context{q};
       beha_log.sub_con1{unit2} = x.s_context{q}; 
       unit2 = unit2+1;
       
    elseif strcmp(x.Behavior{q}, 'Vocalization') && strcmp(x.Start_end{q},'start')
       beha_log.self_vocals{unit3} = 'Vocalization';
       beha_log.self_time{unit3} = x.Time{q};
       beha_log.sub_con2{unit3} = x.s_context{q}; 
       unit3 = unit3+1;
       
    end
    
end
end