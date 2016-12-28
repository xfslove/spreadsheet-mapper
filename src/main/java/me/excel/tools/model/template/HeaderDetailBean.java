package me.excel.tools.model.template;

/**
 * Created by hanwen on 2016/12/28.
 */
public class HeaderDetailBean implements HeaderDetail {

  private String field;

  private String title;

  private String prompt;

  public HeaderDetailBean(String field, String title, String prompt) {
    this.field = field;
    this.title = title;
    this.prompt = prompt;
  }

  @Override
  public String getField() {
    return field;
  }

  @Override
  public String getTitle() {
    return title;
  }

  @Override
  public String getPrompt() {
    return prompt;
  }
}
