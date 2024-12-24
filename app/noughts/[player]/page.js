// app/tictactoe/[player]/page.js
'use client';

import { useState, useEffect, useRef } from 'react';
import styles from '../../../styles/Home.module.css';
import { useParams } from 'next/navigation';

export default function TicTacToePlayer() {
  const { player } = useParams(); // Access dynamic route parameter: 'X' or 'O'

  const [board, setBoard] = useState([]); // State for the current game board
  const [winnerData, setWinnerData] = useState(null); // Data for winning squares or draw
  const [yourTurn, setYourTurn] = useState(false); // Whether it's the current player's turn
  const socketRef = useRef(null);

  useEffect(() => {
    const socket = new WebSocket(`http://localhost:8080/join/${player}`);
    socketRef.current = socket;

    socket.onmessage = (event) => {
      const data = JSON.parse(event.data);

      if (data.board) {
        // Game in progress
        setBoard(data.board);
        setYourTurn(data.currentPlayer);
        setWinnerData(null); // Clear winner or draw state
      } else if (data.won) {
        // Game won
        setWinnerData({ type: 'won', data: data.won });
        setBoard(data.won.map(([pos, [highlight, value]]) => [pos, value]));
      } else if (data.drawn) {
        // Game drawn
        setWinnerData({ type: 'drawn', data: data.drawn });
        setBoard(data.drawn);
      } else if (Array.isArray(data)) {
        // Reset state
        setBoard([]);
        setWinnerData(null);
        setYourTurn(false);
      }
    };

    return () => {
      socket.close();
    };
  }, [player]);

  const handleClick = (index) => {
    if (!yourTurn || board[index][1] || winnerData) return;

    if (socketRef.current && socketRef.current.readyState === WebSocket.OPEN) {
      socketRef.current.send(JSON.stringify(board[index][0])); // Send position directly as "NW", "C", etc.
    }
  };

  return (
    <div className={styles.container}>
      <h1 className={styles.title}>Tic Tac Toe - Player {player}</h1>
      <Board 
        board={board} 
        onClick={handleClick} 
        winnerData={winnerData}
      />
      {winnerData && winnerData.type === 'won' && (
        <p className={styles.winnerMessage}>Winner: {board.find(([_, v]) => v === player)?.[1]}</p>
      )}
      {winnerData && winnerData.type === 'drawn' && (
        <p className={styles.winnerMessage}>It's a Draw!</p>
      )}
      {!winnerData && <p className={styles.turnMessage}>{yourTurn ? "Your Turn" : "Waiting for Opponent"}</p>}
    </div>
  );
}

function Board({ board, onClick, winnerData }) {
  return (
    <div className={styles.board}>
      {board.map(([position, value], index) => (
        <Square
          key={index}
          value={value}
          onClick={() => onClick(index)}
          highlight={winnerData?.type === 'won' && winnerData.data[index]?.[1][0]}
        />
      ))}
    </div>
  );
}

function Square({ value, onClick, highlight }) {
  return (
    <button
      className={`${styles.square} ${highlight ? styles.highlight : ''}`}
      onClick={onClick}
    >
      {value || ''}
    </button>
  );
}
