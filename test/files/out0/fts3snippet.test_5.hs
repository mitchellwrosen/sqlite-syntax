Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "ft"
                }
            , alias = Nothing
            }
        , columns = Nothing
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Values
                        ( Values
                            (
                                ( Expression'Concatenate
                                    ( Expression'Concatenate
                                        ( Expression'Concatenate
                                            ( Expression'Concatenate
                                                ( Expression'Concatenate
                                                    ( Expression'Concatenate
                                                        ( Expression'Concatenate
                                                            ( Expression'LiteralValue
                                                                ( String "one two three four five " )
                                                            )
                                                            ( Expression'LiteralValue
                                                                ( String "six seven eight nine ten " )
                                                            )
                                                        )
                                                        ( Expression'LiteralValue
                                                            ( String "eleven twelve thirteen fourteen fifteen " )
                                                        )
                                                    )
                                                    ( Expression'LiteralValue
                                                        ( String "sixteen seventeen eighteen nineteen twenty " )
                                                    )
                                                )
                                                ( Expression'LiteralValue
                                                    ( String "one two three four five " )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( String "six seven eight nine ten " )
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( String "eleven twelve thirteen fourteen fifteen " )
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "sixteen seventeen eighteen nineteen twenty" )
                                    ) :| []
                                ) :| []
                            )
                        )
                    )
                , orderBy = Nothing
                , limit = Nothing
                }
            ) Nothing
        , returning = Nothing
        }
    )