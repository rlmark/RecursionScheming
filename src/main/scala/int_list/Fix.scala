package int_list

case class Fix[F[_]](unfix: F[Fix[F]]) {

}
object Fix {
  def out[F[_]](fix: Fix[F]): F[Fix[F]] = fix.unfix

  def in[F[_]](f: F[Fix[F]]): Fix[F] = Fix(f)
}