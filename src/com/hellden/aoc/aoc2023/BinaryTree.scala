package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.BinaryTree.{Empty, Matchable, Node}

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

enum BinaryTree[+E : Ordering]:
  private case Empty
  private case Node[+T : Ordering](
    value: T,
    left: BinaryTree[T],
    right: BinaryTree[T],
    depth: Int
  ) extends BinaryTree[T]

  extension [T : Ordering](tree: BinaryTree[T])

    private def depth: Int =
      tree match
        case Empty => 0
        case node: Node[T] => node.depth

    private def removeMin: (BinaryTree[T], Option[T]) =
      tree match
        case Empty => (Empty, None)
        case Node(value, Empty, right, _) => (right, Some(value))
        case node: Node[T] =>
          val (newLeft, value) = removeMin(node.left)
          (Node(node.value, newLeft, node.right, math.min(depth(newLeft), depth(node.right)) + 1), value)

    private def removeMax: (BinaryTree[T], Option[T]) =
      tree match
        case Empty => (Empty, None)
        case Node(value, left, Empty, _) => (left, Some(value))
        case node: Node[T] =>
          val (newRight, value) = removeMax(node.right)
          (Node(node.value, node.left, newRight, math.min(depth(node.left), depth(newRight)) + 1), value)

  private def insert[B >: E : Ordering](value: B): BinaryTree[B] =
    this match
      case Empty =>
        Node(value, Empty, Empty, 1)
      case Node(nodeValue, left, right, _) if value < nodeValue =>
        val (newValue, newLeft, newRight) =
          if depth(left) - depth(right) <= 1 then
            (nodeValue, left.insert(value), right)
          else
            val (newLeft, Some(newNodeValue)) = removeMax(left.insert(value))
            (newNodeValue, newLeft, right.insert(nodeValue))
        Node(newValue, newLeft, newRight, math.max(depth(newLeft), depth(newRight)) + 1)
      case Node(nodeValue, left, right, _) if value > nodeValue =>
        val (newValue, newLeft, newRight) =
          if depth(right) - depth(left) <= 1 then
            (nodeValue, left, right.insert(value))
          else
            val (newRight, Some(newNodeValue)) = removeMin(right.insert(value))
            (newNodeValue, left.insert(nodeValue), newRight)
        Node(newValue, newLeft, newRight, math.max(depth(newLeft), depth(newRight)) + 1)
      case node: Node[_] =>
        node

  def find[K : Ordering, R](key: K)(using matchable: Matchable[E, K]): Option[E] =
    @tailrec
    def rec(tree: BinaryTree[E]): Option[E] =
      tree match
        case Empty => None
        case Node(value, left, right, _) =>
          if value.matching(key) then
            Some(value)
          else if key < value.key then
            rec(left)
          else
            rec(right)
    rec(this)

object BinaryTree:

  trait Matchable[-T, K]:
    extension (t: T)
      def key: K
      def matching(k: K): Boolean

  def apply[T : Ordering](values: Iterable[T]): BinaryTree[T] =
    values.foldLeft[BinaryTree[T]](Empty): (tree, value) =>
      tree.insert(value)
