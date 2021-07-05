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
                                                                                , value = "c"
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
                                                                    ( Expression'And
                                                                        ( Expression'GreaterThanOrEquals
                                                                            ( Expression'Case
                                                                                ( CaseExpression
                                                                                    { base = Nothing
                                                                                    , cases =
                                                                                        ( Expression'Or
                                                                                            ( Expression'GreaterThanOrEquals
                                                                                                ( Expression'Minus
                                                                                                    ( Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Nothing
                                                                                                            , cases =
                                                                                                                ( Expression'Not
                                                                                                                    ( Expression'Not
                                                                                                                        ( Expression'InValues
                                                                                                                            ( InValuesExpression
                                                                                                                                { expression = Expression'BitwiseAnd
                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                        ( Number "11" )
                                                                                                                                    )
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "c"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                , values =
                                                                                                                                    [ Expression'LiteralValue
                                                                                                                                        ( Number "11" )
                                                                                                                                    , Expression'Case
                                                                                                                                        ( CaseExpression
                                                                                                                                            { base = Nothing
                                                                                                                                            , cases =
                                                                                                                                                ( Expression'GreaterThan
                                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                                        ( Number "13" )
                                                                                                                                                    )
                                                                                                                                                    ( Expression'Negate
                                                                                                                                                        ( Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "d"
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                , Expression'Column
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "e"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                ) :|
                                                                                                                                                [
                                                                                                                                                    ( Expression'GreaterThan
                                                                                                                                                        ( Expression'Column
                                                                                                                                                            ( Namespaced
                                                                                                                                                                { namespace = Nothing
                                                                                                                                                                , value = "a"
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
                                                                                                                                                    , Expression'LiteralValue
                                                                                                                                                        ( Number "17" )
                                                                                                                                                    )
                                                                                                                                                ]
                                                                                                                                            , else_ = Expression'Column
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
                                                                                                                                            , value = "b"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                }
                                                                                                                            )
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
                                                                                                                    ( Expression'NotEquals
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "c"
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
                                                                                                                    , Expression'Column
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
                                                                                                                ]
                                                                                                            , else_ = Expression'LiteralValue
                                                                                                                ( Number "19" )
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "b"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'LiteralValue
                                                                                                    ( Number "17" )
                                                                                                )
                                                                                            )
                                                                                            ( Expression'LessThan
                                                                                                ( Expression'LiteralValue
                                                                                                    ( Number "11" )
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
                                                                                        , Expression'Case
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
                                                                                                    ) :|
                                                                                                    [
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
                                                                                                                                                { expression = Expression'LiteralValue
                                                                                                                                                    ( Number "19" )
                                                                                                                                                , values =
                                                                                                                                                    [ Expression'Column
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
                                                                                                                                                    , Expression'Column
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
                                                                                                        , Expression'Column
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
                                                                                                    ]
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
                                                                                        ) :| []
                                                                                    , else_ = Expression'Negate
                                                                                        ( Expression'Negate
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "f"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'LiteralValue
                                                                                ( Number "19" )
                                                                            )
                                                                        )
                                                                        ( Expression'LessThanOrEquals
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "b"
                                                                                    }
                                                                                )
                                                                            )
                                                                            ( Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "e"
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
                                            , Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "d"
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
                    , where_ = Just
                        ( Expression'Not
                            ( Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'Minus
                                        ( Expression'Plus
                                            ( Expression'Plus
                                                ( Expression'Multiply
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "a"
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
                                                )
                                                ( Expression'Minus
                                                    ( Expression'LiteralValue
                                                        ( Number "19" )
                                                    )
                                                    ( Expression'Multiply
                                                        ( Expression'Divide
                                                            ( Expression'FunctionCall
                                                                ( FunctionCallExpression
                                                                    { call = FunctionCall
                                                                        { name = Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "abs"
                                                                            }
                                                                        , arguments = FunctionArguments'Arguments
                                                                            [ Expression'Case
                                                                                ( CaseExpression
                                                                                    { base = Just
                                                                                        ( Expression'Negate
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "c"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                    , cases =
                                                                                        ( Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "d"
                                                                                                }
                                                                                            )
                                                                                        , Expression'Divide
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
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "f"
                                                                                                                    }
                                                                                                                )
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
                                                                                                            [ Expression'Case
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
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , cases =
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "b"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        ) :| []
                                                                                                                    , else_ = Expression'Minus
                                                                                                                        ( Expression'Plus
                                                                                                                            ( Expression'BitwiseNegate
                                                                                                                                ( Expression'Multiply
                                                                                                                                    ( Expression'Negate
                                                                                                                                        ( Expression'Case
                                                                                                                                            ( CaseExpression
                                                                                                                                                { base = Just
                                                                                                                                                    ( Expression'BitwiseOr
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
                                                                                                                                                        )
                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                            ( Number "17" )
                                                                                                                                                        )
                                                                                                                                                    )
                                                                                                                                                , cases =
                                                                                                                                                    ( Expression'Negate
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
                                                                                                                                                    , Expression'Column
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "d"
                                                                                                                                                            }
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
                                                                                                                                                        , value = "e"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "d"
                                                                                                                                            }
                                                                                                                                        )
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
                                                                                                                                    , value = "a"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    }
                                                                                                                )
                                                                                                            ]
                                                                                                        }
                                                                                                    , filter = Nothing
                                                                                                    , over = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        ) :| []
                                                                                    , else_ = Expression'LiteralValue
                                                                                        ( Number "13" )
                                                                                    }
                                                                                )
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
                                                                            [ Expression'LiteralValue
                                                                                ( Number "11" )
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
                                                                , value = "c"
                                                                }
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "17" )
                                            )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "d"
                                                }
                                            )
                                        )
                                    , values =
                                        [ Expression'LiteralValue
                                            ( Number "11" )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
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