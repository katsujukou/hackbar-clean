import { defineConfig } from "vite";
import dns from "dns";

dns.setDefaultResultOrder("ipv4first");

export default defineConfig({
  server: {
    host: true,
    proxy: {
      '/api': {
        target: 'http://localhost:3000/',
        changeOrigin: true,
        rewrite: path => path.replace(/^\/api/, '')
      }
    }
  }
});