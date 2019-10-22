import seaborn as sns
import matplotlib.pyplot as plt

def draw_hist(data,bins):
  plot = sns.distplot(data, bins)
  #df = sns.load_dataset('iris')
  #sns.distplot(df["sepal_length"], bins=20 )
  plt.savefig('myplot.png')
  #return plot
  #plt.hist(df["sepal_length"], normed=True, bins=30)
  #plt.show()
