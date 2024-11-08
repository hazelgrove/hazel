import asyncio
import websockets
from websockets.server import WebSocketServerProtocol
from typing import Set

connected_clients: Set[WebSocketServerProtocol] = set()

async def echo(websocket: WebSocketServerProtocol, path: str) -> None:
    print("Client connected")
    connected_clients.add(websocket)
    try:
        async for message in websocket:
            print(f"Received message from client: {message}")
            if connected_clients:
                await asyncio.gather(*(client.send(f"{message}") for client in connected_clients if client != websocket))
            else:
                print("No clients connected")
    except websockets.ConnectionClosed:
        print("Client disconnected")
    finally:
        connected_clients.remove(websocket)

async def console_input() -> None:
    while True:
        message = await asyncio.get_event_loop().run_in_executor(None, input, "Enter message to broadcast: ")
        if connected_clients:
            await asyncio.gather(*(client.send(f"{message}") for client in connected_clients))
        else:
            print("No clients connected")

async def main() -> None:
    async with websockets.serve(echo, "localhost", 8765):
        print("WebSocket server running on ws://localhost:8765")
        await asyncio.gather(console_input(), asyncio.Future())  # Run console input and server forever

if __name__ == "__main__":
    asyncio.run(main())
