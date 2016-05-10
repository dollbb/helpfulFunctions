import pandas as pd
import statsmodels.formula.api as smf
from scipy.stats import ttest_1samp

def lm_rep(formula, dat, by):
    '''
    repeat a linear regression on subsets of a DataFrame
    formula = regression formula string. e.g. 'y ~ x'
    data = pandas DataFrame
    by = column name string indicating data subset for lm e.g. 'subject'
    '''
    units = dat[by].unique()
    coefs = pd.DataFrame()
    for unit in units:
        sdat = dat.loc[dat[by] == unit]
        res = smf.ols(formula, data = sdat).fit()
        scoefs = res.params
        scoefs.name = unit
        coefs = pd.concat([coefs,scoefs], axis = 1)

    coefs = coefs.T
    return(coefs)

 

def group_stats(dat, dif=0):
    '''compute summary stats for lm_rep output DataFrame'''
    mean = dat.mean()
    mean.name = 'mean'
    sem = dat.std() / pow(len(dat), 0.5)
    sem.name = 'sem'
    t = dat.mean() / (dat.std() / pow(len(dat), 0.5))
    t.name = 't'
    p = ttest_1samp(dat, 0)[1]
    p = pd.Series(p, index = mean.index.values.tolist())
    p.name = 'p'
    out = pd.concat([mean, sem, t, p], axis=1)
    return(out)
