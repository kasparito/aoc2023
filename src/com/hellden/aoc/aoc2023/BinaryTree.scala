package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.BinaryTree.MatchResult

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

enum BinaryTree[+A : Ordering]:
  private case Empty
  private case Node[+T : Ordering](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

  private def insert[B >: A : Ordering](value: B): BinaryTree[B] =
    this match
      case Empty =>
        Node(value, Empty, Empty)
      case node: Node[_] if value < node.value =>
        node.copy(left = node.left.insert(value))
      case node: Node[_] if value > node.value =>
        node.copy(right = node.right.insert(value))
      case node: Node[_] =>
        node

  def find[R](fun: A => MatchResult[R]): Option[R] =
    BinaryTree.find(fun, this)

object BinaryTree:
  import MatchResult._

  enum MatchResult[+R]:
    case Left
    case Right
    case Match[T](result: T) extends MatchResult[T]

  def apply[T : Ordering](values: Iterable[T]): BinaryTree[T] =
    values.foldLeft[BinaryTree[T]](Empty): (tree, value) =>
      tree.insert(value)

  @tailrec
  private def find[T, R](fun: T => MatchResult[R], tree: BinaryTree[T]): Option[R] =
    tree match
      case Empty => None
      case node: Node[T] =>
        fun(node.value) match
          case Left =>
            find(fun, node.left)
          case Right =>
            find(fun, node.right)
          case Match(result) =>
            Some(result)
