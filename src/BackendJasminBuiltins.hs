module BackendJasminBuiltins where

builtinMethods = [
    ".method public <init>()V",
    "    aload_0",
    "    invokespecial java/lang/Object/<init>()V",
    "    return",
    ".end method",
    "",
    ".method public static printInt(I)V",
    "    getstatic java/lang/System/out Ljava/io/PrintStream;",
    "    iload_0",
    "    invokevirtual java/io/PrintStream/println(I)V",
    "    return",
    ".end method",
    "",
    ".method public static printString(Ljava/lang/String;)V",
    "    getstatic java/lang/System/out Ljava/io/PrintStream;",
    "    aload_0",
    "    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V",
    "    return",
    ".end method",
    "",
    ".method public static error()",
    "    getstatic java/lang/System/err Ljava/io/PrintStream;",
    "    ldc \"runtime error\"",
    "    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V",
    "    iconst_1",
    "    invokestatic java/lang/System/exit(I)V",
    "    return",
    ".end method",
    "",
    ".method public static readInt()I",
    ".catch java/io/IOException from try_start to try_end using failure",
    "    iconst_0",
    "    istore_0",
    "  try_start:",
    "    new java/io/BufferedReader",
    "    dup",
    "    new java/io/InputStreamReader",
    "    dup",
    "    getstatic java/lang/System/in Ljava/io/InputStream;",
    "    invokespecial java/io/InputStreamReader/<init>(Ljava/io/InputStream;)V",
    "    invokespecial java/io/BufferedReader/<init>(Ljava/io/Reader;)V",
    "    astore_1",
    "    aload_1",
    "    invokevirtual java/io/BufferedReader/readLine()Ljava/lang/String;",
    "    astore_2",
    "    aload_2",
    "    invokestatic java/lang/Integer/parseInt(Ljava/lang/String;)I",
    "    istore_0",
    "  try_end:",
    "    goto end",
    "  failure:",
    "    astore_1",
    "  end:",
    "    iload_0",
    "    ireturn",
    ".end method",
    "",
    ".method public static readString()Ljava/lang/String;",
    ".catch java/io/IOException from try_start to try_end using failure",
    "    ldc \"\"",
    "    astore_0",
    "  try_start:",
    "    new java/io/InputStreamReader",
    "    dup",
    "    getstatic java/lang/System/in Ljava/io/InputStream;",
    "    invokespecial java/io/InputStreamReader/<init>(Ljava/io/InputStream;)V",
    "    astore_1",
    "    new java/io/BufferedReader",
    "    dup",
    "    aload_1",
    "    invokespecial java/io/BufferedReader/<init>(Ljava/io/Reader;)V",
    "    astore_2",
    "    aload_2",
    "    invokevirtual java/io/BufferedReader/readLine()Ljava/lang/String;",
    "    astore_0",
    "  try_end:",
    "    goto end",
    "  failure:",
    "    astore_1",
    "  end:",
    "    aload_0",
    "    areturn",
    ".end method",
    "",
    "",
    ".method public static strComp(Ljava/lang/String;Ljava/lang/String;)I",
    "    aload_0",
    "    aload_1",
    "    invokevirtual java/lang/String/equals(Ljava/lang/Object;)Z",
    "    ifeq else",
    "    iconst_1",
    "    ireturn",
    "  else:",
    "    iconst_0",
    "    ireturn",
    ".end method",
    "",
    ".method public static strConcat(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;",
    "    aload_0",
    "    astore_2",
    "    aload_2",
    "    aload_1",
    "    invokevirtual java/lang/String/concat(Ljava/lang/String;)Ljava/lang/String;",
    "    pop",
    "    aload_2",
    "    areturn",
    ".end method",
    "",
    ".method public static modulo(II)I",
    "    iload_0",
    "    iload_1",
    "    irem",
    "    ireturn",
    ".end method"]
