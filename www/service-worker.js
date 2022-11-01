// Incrementing OFFLINE_VERSION will kick off the install 
// event and force previously cached resources to be 
// updated from the network.
const OFFLINE_VERSION = 1;
const CACHE_NAME = 'offline';
// Customize this with a different URL if needed.
const OFFLINE_URL = 'offline.html';

self.addEventListener('install', (event) => {
  // Install logic
});

self.addEventListener('activate', (event) => {
  // Activate logic
});

self.addEventListener('fetch', (event) => {
  // Fetch logic
});