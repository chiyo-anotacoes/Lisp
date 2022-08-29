import { defineConfig } from "vite";
import reactRefresh from "@vitejs/plugin-react-refresh";

export default defineConfig({
  plugins: [reactRefresh({ include: "**/*.bs.js", exclude: "/node_modules/" })],
});