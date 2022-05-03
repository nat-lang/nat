import React, { forwardRef, useState, useEffect } from 'react';
import { drag } from 'd3-drag';
import { select } from 'd3-selection';
import { ID } from 'types';
import { getTextDimensions } from 'utils/document';
import { NodeDragHandler, CoordinatedTreeNode, NodeDragEvent } from './types';
import { nodeText } from './utils';
import { NODE_RADIUS } from './config';

type NodeProps = {
  treeId: ID
  node: CoordinatedTreeNode
  className?: string
  width: number
  height: number
  onClick: React.MouseEventHandler
  onDragProceed: NodeDragHandler
  onDragEnd: NodeDragHandler
}

const Node = forwardRef<
  SVGGElement, NodeProps
>(({ treeId, node, onClick, onDragProceed, onDragEnd, className = "" }, ref) => {
  const { id: nodeId } = node.data;
  const text = nodeText(node);
  const id = `${treeId}-${nodeId}`;
  const { width: textWidth, height: textHeight } = getTextDimensions(text);
  const textX = node.x - textWidth / 2, textY = node.y + textHeight / 2;
  const [latestDragEvent, setLatestDragEvent] = useState<NodeDragEvent | null>(null);

  const dragHandler = drag();

  useEffect(() => {
    const selection = select<Element, any>(`g[data-id="${id}"]`);

    dragHandler(selection);
  
    dragHandler.on("drag", (e) => {
      onDragProceed(e);
      setLatestDragEvent(e);
    });
  }, []);

  // see https://github.com/d3/d3-drag#drag_on for why this contortion is necessary
  // briefly: the callback needs to be re-registered on each event in order to be fresh
  useEffect(() => {
    latestDragEvent?.on("end", onDragEnd);
  }, [onDragEnd, latestDragEvent])

  return (
    <g ref={ref} onClick={onClick} className={`node ${className}`} data-id={id}>
      <circle className="node-circle" cx={node.x} cy={node.y} r={NODE_RADIUS} fill="white" strokeWidth="1"/>
      <text x={textX} y={textY} data-id={textWidth}>
        {text}
      </text>
    </g>
  );
});

export default Node