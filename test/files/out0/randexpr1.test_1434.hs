Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Case
                                ( CaseExpression
                                    { base = Nothing
                                    , cases =
                                        ( Expression'LessThan
                                            ( Expression'Divide
                                                ( Expression'FunctionCall
                                                    ( FunctionCallExpression
                                                        { call = FunctionCall
                                                            { name = Namespaced
                                                                { namespace = Nothing
                                                                , value = "abs"
                                                                }
                                                            , arguments = FunctionArguments'Arguments
                                                                [ Expression'LiteralValue
                                                                    ( Number "17" )
                                                                ]
                                                            }
                                                        , filter = Nothing
                                                        , over = Nothing
                                                        }
                                                    )
                                                )
                                                ( Expression'FunctionCall
                                                    ( FunctionCallExpression
                                                        { call = FunctionCall
                                                            { name = Namespaced
                                                                { namespace = Nothing
                                                                , value = "abs"
                                                                }
                                                            , arguments = FunctionArguments'Arguments
                                                                [ Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Just
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "t1"
                                                                                }
                                                                            )
                                                                        , value = "e"
                                                                        }
                                                                    )
                                                                ]
                                                            }
                                                        , filter = Nothing
                                                        , over = Nothing
                                                        }
                                                    )
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "b"
                                                    }
                                                )
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "t1"
                                                        }
                                                    )
                                                , value = "d"
                                                }
                                            )
                                        ) :|
                                        [
                                            ( Expression'Not
                                                ( Expression'InValues
                                                    ( InValuesExpression
                                                        { expression = Expression'Subquery
                                                            ( SelectStatement
                                                                { commonTableExpressions = Nothing
                                                                , select = CompoundSelect
                                                                    ( SelectCore'Select
                                                                        ( Select
                                                                            { distinct = False
                                                                            , columns = ResultColumn'Expression
                                                                                ( Aliased
                                                                                    { value = Expression'AggregateDistinctFunctionCall
                                                                                        ( AggregateDistinctFunctionCallExpression
                                                                                            { call = FunctionCall
                                                                                                { name = Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "count"
                                                                                                    }
                                                                                                , arguments = Identity
                                                                                                    ( Expression'FunctionCall
                                                                                                        ( FunctionCallExpression
                                                                                                            { call = FunctionCall
                                                                                                                { name = Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "coalesce"
                                                                                                                    }
                                                                                                                , arguments = FunctionArguments'Arguments
                                                                                                                    [ Expression'Subquery
                                                                                                                        ( SelectStatement
                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                            , select = CompoundSelect
                                                                                                                                ( SelectCore'Select
                                                                                                                                    ( Select
                                                                                                                                        { distinct = False
                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                            ( Aliased
                                                                                                                                                { value = Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "d"
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
                                                                                                                                        , where_ = Just
                                                                                                                                            ( Expression'Exists
                                                                                                                                                ( SelectStatement
                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                            ( Select
                                                                                                                                                                { distinct = False
                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                    ( Aliased
                                                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                                                            ( Number "1" )
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
                                                                                                                                                                , where_ = Just
                                                                                                                                                                    ( Expression'LessThan
                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                            ( Number "19" )
                                                                                                                                                                        )
                                                                                                                                                                        ( Expression'Multiply
                                                                                                                                                                            ( Expression'Subquery
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
                                                                                                                                                                                                                        , value = "abs"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                        [ Expression'FunctionCall
                                                                                                                                                                                                                            ( FunctionCallExpression
                                                                                                                                                                                                                                { call = FunctionCall
                                                                                                                                                                                                                                    { name = Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "max"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                , value = "b"
                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                        ]
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                , filter = Nothing
                                                                                                                                                                                                                                , over = Nothing
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
                                                                                                                                                                            )
                                                                                                                                                                            ( Expression'Case
                                                                                                                                                                                ( CaseExpression
                                                                                                                                                                                    { base = Nothing
                                                                                                                                                                                    , cases =
                                                                                                                                                                                        ( Expression'GreaterThanOrEquals
                                                                                                                                                                                            ( Expression'Column
                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                            ( Expression'Case
                                                                                                                                                                                                ( CaseExpression
                                                                                                                                                                                                    { base = Nothing
                                                                                                                                                                                                    , cases =
                                                                                                                                                                                                        ( Expression'Exists
                                                                                                                                                                                                            ( SelectStatement
                                                                                                                                                                                                                { commonTableExpressions = Nothing
                                                                                                                                                                                                                , select = CompoundSelect
                                                                                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                                                                                        ( Select
                                                                                                                                                                                                                            { distinct = False
                                                                                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                                                                                ( Aliased
                                                                                                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                                                                                                        ( Number "1" )
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
                                                                                                                                                                                                                            , where_ = Just
                                                                                                                                                                                                                                ( Expression'Not
                                                                                                                                                                                                                                    ( Expression'Exists
                                                                                                                                                                                                                                        ( SelectStatement
                                                                                                                                                                                                                                            { commonTableExpressions = Nothing
                                                                                                                                                                                                                                            , select = CompoundSelect
                                                                                                                                                                                                                                                ( SelectCore'Select
                                                                                                                                                                                                                                                    ( Select
                                                                                                                                                                                                                                                        { distinct = False
                                                                                                                                                                                                                                                        , columns = ResultColumn'Expression
                                                                                                                                                                                                                                                            ( Aliased
                                                                                                                                                                                                                                                                { value = Expression'LiteralValue
                                                                                                                                                                                                                                                                    ( Number "1" )
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
                                                                                                                                                                                                                                                        , where_ = Just
                                                                                                                                                                                                                                                            ( Expression'NotEquals
                                                                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                                                                                    ( Number "19" )
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
                                                                                                                                                                                                        , Expression'Plus
                                                                                                                                                                                                            ( Expression'FunctionCall
                                                                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "coalesce"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                                                                            [ Expression'Subquery
                                                                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                                                                            ( Select
                                                                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                                                                                                                                            ( Number "11" )
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
                                                                                                                                                                                                                                                , where_ = Just
                                                                                                                                                                                                                                                    ( Expression'Not
                                                                                                                                                                                                                                                        ( Expression'Exists
                                                                                                                                                                                                                                                            ( SelectStatement
                                                                                                                                                                                                                                                                { commonTableExpressions = Nothing
                                                                                                                                                                                                                                                                , select = CompoundSelect
                                                                                                                                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                                                                                                                                        ( Select
                                                                                                                                                                                                                                                                            { distinct = False
                                                                                                                                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                                                                                                                                ( Aliased
                                                                                                                                                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                                                                                                                                                        ( Number "1" )
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
                                                                                                                                                                                                                                                                            , where_ = Just
                                                                                                                                                                                                                                                                                ( Expression'Not
                                                                                                                                                                                                                                                                                    ( Expression'InValues
                                                                                                                                                                                                                                                                                        ( InValuesExpression
                                                                                                                                                                                                                                                                                            { expression = Expression'Column
                                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                    , value = "e"
                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                            , values =
                                                                                                                                                                                                                                                                                                [ Expression'Negate
                                                                                                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                                                                                                )
                                                                                                                                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                                                                                        , value = "b"
                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                , Expression'Column
                                                                                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                                                                                                }
                                                                                                                                                                                                                                                                                                            )
                                                                                                                                                                                                                                                                                                        , value = "d"
                                                                                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                                                                                ]
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
                                                                                                                                                                                                                            , Expression'Column
                                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                                    { namespace = Just
                                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                                            , value = "t1"
                                                                                                                                                                                                                                            }
                                                                                                                                                                                                                                        )
                                                                                                                                                                                                                                    , value = "a"
                                                                                                                                                                                                                                    }
                                                                                                                                                                                                                                )
                                                                                                                                                                                                                            ]
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                                                                    , over = Nothing
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            )
                                                                                                                                                                                                            ( Expression'Multiply
                                                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                                                        }
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                )
                                                                                                                                                                                                                ( Expression'LiteralValue
                                                                                                                                                                                                                    ( Number "11" )
                                                                                                                                                                                                                )
                                                                                                                                                                                                            )
                                                                                                                                                                                                        ) :| []
                                                                                                                                                                                                    , else_ = Expression'Column
                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                                                    }
                                                                                                                                                                                                                )
                                                                                                                                                                                                            , value = "a"
                                                                                                                                                                                                            }
                                                                                                                                                                                                        )
                                                                                                                                                                                                    }
                                                                                                                                                                                                )
                                                                                                                                                                                            )
                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "b"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        ) :| []
                                                                                                                                                                                    , else_ = Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    }
                                                                                                                                                                                )
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
                                                                                                                    , Expression'LiteralValue
                                                                                                                        ( Number "17" )
                                                                                                                    ]
                                                                                                                }
                                                                                                            , filter = Nothing
                                                                                                            , over = Nothing
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                }
                                                                                            , filter = Nothing
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
                                                        , values =
                                                            [ Expression'LiteralValue
                                                                ( Number "13" )
                                                            , Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "e"
                                                                    }
                                                                )
                                                            , Expression'LiteralValue
                                                                ( Number "17" )
                                                            ]
                                                        }
                                                    )
                                                )
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                        ]
                                    , else_ = Expression'LiteralValue
                                        ( Number "11" )
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
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'Not
                                ( Expression'Or
                                    ( Expression'Or
                                        ( Expression'NotEquals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "coalesce"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Subquery
                                                                ( SelectStatement
                                                                    { commonTableExpressions = Nothing
                                                                    , select = CompoundSelect
                                                                        ( SelectCore'Select
                                                                            ( Select
                                                                                { distinct = False
                                                                                , columns = ResultColumn'Expression
                                                                                    ( Aliased
                                                                                        { value = Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Just
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Just
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "t1"
                                                                                                                    }
                                                                                                                )
                                                                                                            , value = "c"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                , cases =
                                                                                                    ( Expression'Minus
                                                                                                        ( Expression'Multiply
                                                                                                            ( Expression'FunctionCall
                                                                                                                ( FunctionCallExpression
                                                                                                                    { call = FunctionCall
                                                                                                                        { name = Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "coalesce"
                                                                                                                            }
                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                            [ Expression'Subquery
                                                                                                                                ( SelectStatement
                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                    , select = CompoundSelect
                                                                                                                                        ( SelectCore'Select
                                                                                                                                            ( Select
                                                                                                                                                { distinct = False
                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                    ( Aliased
                                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                                            ( Number "19" )
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
                                                                                                                                                , where_ = Just
                                                                                                                                                    ( Expression'Or
                                                                                                                                                        ( Expression'InValues
                                                                                                                                                            ( InValuesExpression
                                                                                                                                                                { expression = Expression'Case
                                                                                                                                                                    ( CaseExpression
                                                                                                                                                                        { base = Nothing
                                                                                                                                                                        , cases =
                                                                                                                                                                            ( Expression'Not
                                                                                                                                                                                ( Expression'Exists
                                                                                                                                                                                    ( SelectStatement
                                                                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                                                                        , select = CompoundSelect
                                                                                                                                                                                            ( SelectCore'Select
                                                                                                                                                                                                ( Select
                                                                                                                                                                                                    { distinct = False
                                                                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                                                                        ( Aliased
                                                                                                                                                                                                            { value = Expression'LiteralValue
                                                                                                                                                                                                                ( Number "1" )
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
                                                                                                                                                                                                    , where_ = Just
                                                                                                                                                                                                        ( Expression'Not
                                                                                                                                                                                                            ( Expression'InValues
                                                                                                                                                                                                                ( InValuesExpression
                                                                                                                                                                                                                    { expression = Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    , values =
                                                                                                                                                                                                                        [ Expression'Column
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                , value = "c"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                                                , value = "e"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        , Expression'Column
                                                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                                                                        }
                                                                                                                                                                                                                                    )
                                                                                                                                                                                                                                , value = "f"
                                                                                                                                                                                                                                }
                                                                                                                                                                                                                            )
                                                                                                                                                                                                                        ]
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
                                                                                                                                                                                )
                                                                                                                                                                            , Expression'BitwiseOr
                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , value = "f"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                                ( Expression'Column
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Just
                                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                                                , value = "t1"
                                                                                                                                                                                                }
                                                                                                                                                                                            )
                                                                                                                                                                                        , value = "a"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                )
                                                                                                                                                                            ) :| []
                                                                                                                                                                        , else_ = Expression'Column
                                                                                                                                                                            ( Namespaced
                                                                                                                                                                                { namespace = Just
                                                                                                                                                                                    ( Namespaced
                                                                                                                                                                                        { namespace = Nothing
                                                                                                                                                                                        , value = "t1"
                                                                                                                                                                                        }
                                                                                                                                                                                    )
                                                                                                                                                                                , value = "a"
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        }
                                                                                                                                                                    )
                                                                                                                                                                , values =
                                                                                                                                                                    [ Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "d"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                                        ( Number "19" )
                                                                                                                                                                    , Expression'Column
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Just
                                                                                                                                                                                ( Namespaced
                                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                                    , value = "t1"
                                                                                                                                                                                    }
                                                                                                                                                                                )
                                                                                                                                                                            , value = "a"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    ]
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                        ( Expression'LessThanOrEquals
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "f"
                                                                                                                                                                    }
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Just
                                                                                                                                                                        ( Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "t1"
                                                                                                                                                                            }
                                                                                                                                                                        )
                                                                                                                                                                    , value = "c"
                                                                                                                                                                    }
                                                                                                                                                                )
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
                                                                                                                            , Expression'LiteralValue
                                                                                                                                ( Number "13" )
                                                                                                                            ]
                                                                                                                        }
                                                                                                                    , filter = Nothing
                                                                                                                    , over = Nothing
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Just
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "t1"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Just
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , value = "d"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    , Expression'Negate
                                                                                                        ( Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Just
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "t1"
                                                                                                                        }
                                                                                                                    )
                                                                                                                , value = "c"
                                                                                                                }
                                                                                                            )
                                                                                                        )
                                                                                                    ) :| []
                                                                                                , else_ = Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
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
                                                                                , where_ = Just
                                                                                    ( Expression'LessThan
                                                                                        ( Expression'LiteralValue
                                                                                            ( Number "17" )
                                                                                        )
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "f"
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
                                                            , Expression'Negate
                                                                ( Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "e"
                                                                        }
                                                                    )
                                                                )
                                                            ]
                                                        }
                                                    , filter = Nothing
                                                    , over = Nothing
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'LessThanOrEquals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "a"
                                                    }
                                                )
                                            )
                                        )
                                    )
                                    ( Expression'And
                                        ( Expression'LessThan
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Just
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "t1"
                                                            }
                                                        )
                                                    , value = "e"
                                                    }
                                                )
                                            )
                                        )
                                        ( Expression'Between
                                            ( Expression'Negate
                                                ( Expression'LiteralValue
                                                    ( Number "13" )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "19" )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "19" )
                                            )
                                        )
                                    )
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