Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Collate
                                ( CollateExpression
                                    { expression = Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "a"
                                            }
                                        )
                                    , collation = "nocase"
                                    }
                                )
                            , alias = Just "x"
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t2"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'Equals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "x"
                                    }
                                )
                            )
                            ( Expression'Collate
                                ( CollateExpression
                                    { expression = Expression'LiteralValue
                                        ( String "abc" )
                                    , collation = "binary"
                                    }
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )