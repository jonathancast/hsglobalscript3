mapM0_gsio.monad (λ 'a. analyzeM_gsio.monad gsio.file.stat (file.name.in a).
    case right 'st. (analyze st #is.dir.
        case true. gsio.dir.read (file.name.in a) >>=_gsio.monad mapM0_gsio.monad (λ 'a1. analyze file.name.extension.get a1.
        ),
    ),
)
