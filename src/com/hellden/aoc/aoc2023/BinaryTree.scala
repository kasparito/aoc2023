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

  def find[K : Ordering, R](key: K)(using matchable: Matchable[E, K, R]): Option[R] =
    @tailrec
    def rec(tree: BinaryTree[E]): Option[R] =
      tree match
        case Empty => None
        case Node(value, left, right) =>
          value.matching(key) match
            case result: Some[_] =>
              result
            case _ if key < value.key =>
              rec(left)
            case _ =>
              rec(right)
    rec(this)

object BinaryTree:

  trait Matchable[-T, K, R]:
    extension (t: T)
      def key: K
      def matching(k: K): Option[R]

  def apply[T : Ordering](values: Iterable[T]): BinaryTree[T] =
    values.foldLeft[BinaryTree[T]](Empty): (tree, value) =>
      tree.insert(value)
