package me.excel.tools;

import me.excel.tools.model.template.HeaderDetail;
import me.excel.tools.model.template.HeaderDetailBean;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2016/12/28.
 */
public class HeaderDetailBuilder {

  private List<HeaderDetail> details = new ArrayList<>();

  public List<HeaderDetail> build() {
    return details;
  }

  public HeaderDetailBuilder header(String field, String title, String prompt) {
    details.add(new HeaderDetailBean(field, title, prompt));
    return this;
  }

}
