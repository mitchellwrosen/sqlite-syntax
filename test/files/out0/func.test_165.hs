Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "group_concat"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Case
                                                ( CaseExpression
                                                    { base = Just
                                                        ( Expression'Column
                                                            ( Namespaced
                                                                { namespace = Nothing
                                                                , value = "t1"
                                                                }
                                                            )
                                                        )
                                                    , cases =
                                                        ( Expression'LiteralValue
                                                            ( String "this" )
                                                        , Expression'LiteralValue Null
                                                        ) :| []
                                                    , else_ = Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    }
                                                )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "tbl1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )