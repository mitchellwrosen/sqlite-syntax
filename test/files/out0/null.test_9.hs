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
                                            , value = "ifnull"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Case
                                                ( CaseExpression
                                                    { base = Nothing
                                                    , cases =
                                                        ( Expression'Not
                                                            ( Expression'Or
                                                                ( Expression'NotEquals
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "0" )
                                                                    )
                                                                )
                                                                ( Expression'NotEquals
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "c"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'LiteralValue
                                                                        ( Number "0" )
                                                                    )
                                                                )
                                                            )
                                                        , Expression'LiteralValue
                                                            ( Number "1" )
                                                        ) :| []
                                                    , else_ = Expression'LiteralValue
                                                        ( Number "0" )
                                                    }
                                                )
                                            , Expression'LiteralValue
                                                ( Number "99" )
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
                                        , value = "t1"
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