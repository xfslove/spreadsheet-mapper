package me.excel.tools.prompter;

/**
 * boolean prompter
 * <p>
 * Created by hanwen on 2016/12/19.
 */
public class BooleanPrompter extends DefaultFieldPrompter {

  public BooleanPrompter(String matchField) {
    super(matchField,
        "\'是\':true,t,是,yes,y,1; \'否\':false,f,否,no,n,0");
  }

}
