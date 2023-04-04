% Function for smoothing firing rate when time bin is 1 s
function [smooth_fr] = smoothingFR1(x,sigma)
gauss_range = -3*sigma:3*sigma; % calculate 3 stds out, use same resolution for convenience
smoothing_kernel = normpdf(gauss_range,0,sigma); % Set up Gaussian kernel
smoothing_kernel = smoothing_kernel /sum(smoothing_kernel); % Normalizing; sum = 1
%smoothing_kernel = smoothing_kernel * 1; % Rescale to get correct firing rate: ?
Spike_rasters_smooth = conv2(x, smoothing_kernel,'same'); % Applying convolution
smooth_fr = Spike_rasters_smooth;

% Plotting the first row and first 100 neurons to see the results of
% smoothing
figure (1)
plot(x(1,1:100));
hold on
plot(smooth_fr(1,1:100));
legend("Before smoothing", "Gaussian smoothing");
hold off

end
