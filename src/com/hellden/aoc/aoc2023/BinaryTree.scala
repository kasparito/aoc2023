package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.BinaryTree.Matchable

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

enum BinaryTree[+E : Ordering]:
  private case Empty
  private case Node[+T : Ordering](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

  private def insert[B >: E : Ordering](value: B): BinaryTree[B] =
    this match
      case Empty =>
        Node(value, Empty, Empty)
      case node: Node[_] if value < node.value =>
        node.copy(left = node.left.insert(value))
      case node: Node[_] if value > node.value =>
        node.copy(right = node.right.insert(value))
      case node: Node[_] =>
        node

  def find[K : Ordering, R](key: K)(using matchable: Matchable[E, K]): Option[E] =
    @tailrec
    def rec(tree: BinaryTree[E]): Option[E] =
      tree match
        case Empty => None
        case Node(value, left, right) =>
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
