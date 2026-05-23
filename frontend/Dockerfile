# --- build stage ---
FROM node:22-alpine AS build
WORKDIR /app

COPY package*.json ./

RUN pnpm install --frozen-lockfile

COPY . .

RUN pnpm run build

# --- run stage ---
FROM node:22-alpine
WORKDIR /app

ENV NODE_ENV=production

COPY --from=build /app ./

EXPOSE 3000
CMD ["pnpm", "start"]
