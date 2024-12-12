import asyncio
import websockets
import json

PORT = 8765

connected_clients = set()
game_state = {
    "players": {}
}

async def handler(websocket, path):
    connected_clients.add(websocket)
    player_id = None
    try:
        async for message in websocket:
            data = json.loads(message)
            if data['type'] == 'join':
                player_id = data['player']['id']
                game_state['players'][player_id] = data['player']
                await broadcast_game_state()
            elif data['type'] == 'move' and player_id is not None:
                game_state['players'][player_id] = data['player']
                await broadcast_game_state()
    except websockets.exceptions.ConnectionClosed:
        pass
    finally:
        if player_id is not None and player_id in game_state['players']:
            del game_state['players'][player_id]
            await broadcast_game_state()
        connected_clients.remove(websocket)

async def broadcast_game_state():
    if connected_clients:
        message = json.dumps({"type": "state", "state": game_state})
        # Create tasks explicitly for each send operation
        await asyncio.gather(*(asyncio.create_task(client.send(message)) for client in connected_clients))

start_server = websockets.serve(handler, "localhost", PORT)

asyncio.get_event_loop().run_until_complete(start_server)
asyncio.get_event_loop().run_forever()
