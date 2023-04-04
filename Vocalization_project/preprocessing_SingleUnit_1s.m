%% Preprocessing Neural data
Chan_name = fieldnames(SpikeData); %Identify channel names
C = regexp(Chan_name,'\d*','Match');
C_char = cellfun(@char, C{:}, 'UniformOutput', false);
Chan_num = str2num(C_char{1, 1});
%Separate channels by array
%IMPORTANT NOTE: the channel mapping is reversed for each subject
%This mapping is for A
    vlPFC_chan = [1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30,33,34,37,38,41,...
        42,45,46,49,50,53,54,57,58,61,62,65,66,69,70,73,74,77,78,81,82,85,86,...
        89,90,93,94,97,98,101,102,105,106,109,110,113,114,117,118,121,122,125,126];
    TEO_chan = [3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32,35,36,39,40,...
        43,44,47,48,51,52,55,56,59,60,63,64,67,68,71,72,75,76,79,80,83,84,...
        87,88,91,92,95,96,99,100,103,104,107,108,111,112,115,116,119,120,123,124,127,128];
chan_idx_TEO = find(ismember(Chan_num,TEO_chan))';
chan_idx_vlPFC = find(ismember(Chan_num,vlPFC_chan))';

%Select channels
channel_flag = input('which region you want to analyze: ');
if strcmp(channel_flag,'TEO') %If you want TEO
    channels = chan_idx_TEO;
elseif strcmp(channel_flag,'vlPFC') %vlPFC
    channels = chan_idx_vlPFC;
elseif strcmp(channel_flag,'all') 
    channels = 1:length(fields(SpikeData)); %all channels
end

%Create spike matrix structure
length_recording = cell2mat(a729.Time(length(a729.Time))); % get length of recording for each session
temp_resolution = 0.001; % time bin = 1s
 
unit=1;
for i = channels %For all channels
    if ~isempty(SpikeData.(Chan_name{i})) %If there are sorted units on this channel
        for j = 1:length(SpikeData.(Chan_name{i})) %For all units
            Spike_rasters729a_pfc1(unit,:) = zeros(1,round(length_recording*temp_resolution)); 
            
            ticks = round( SpikeData.(Chan_name{i}){j});  
            Spike_counts = hist(ticks, (1:round(length_recording*temp_resolution))); 
            Spike_rasters729a_pfc1(unit, :) = Spike_counts; %Fill in spikes in the raster
            if ismember(Chan_num(i),TEO_chan)
                brain_label(unit) = "TEO";
            else
                brain_label(unit) = "vlPFC";
            end
            clear ticks Spike_counts
            unit = unit+1;
        end
    end
end

 


    
    
    




















