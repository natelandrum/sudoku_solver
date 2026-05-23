# --- build stage ---
FROM node:22-alpine AS build
WORKDIR /app

RUN corepack enable

COPY package*.json pnpm-lock.yaml pnpm-workspace.yaml ./

RUN pnpm install --frozen-lockfile

COPY . .

RUN pnpm run build

# --- run stage ---
FROM node:22-alpine
WORKDIR /app

RUN corepack enable

ENV NODE_ENV=production

COPY --from=build /app ./

EXPOSE 3000
CMD ["pnpm", "start"]
