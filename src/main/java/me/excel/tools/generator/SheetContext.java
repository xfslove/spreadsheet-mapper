package me.excel.tools.generator;

import me.excel.tools.model.excel.Sheet;
import me.excel.tools.model.template.HeaderDetail;
import me.excel.tools.model.template.SheetHeader;
import me.excel.tools.model.template.SheetHeaderBean;

import java.util.List;

/**
 * the context for write on excel sheet.
 * <p>
 * Created by hanwen on 2016/12/28.
 */
public class SheetContext {

  private int sheetIndex;

  private String sheetName;

  private SheetHeader header;

  private List<HeaderDetail> headerDetails;

  private List<Object> data;

  public SheetContext(List<HeaderDetail> headerDetails, List<Object> data) {
    this.sheetIndex = 1;
    this.header = SheetHeaderBean.DEFAULT(1);
    this.headerDetails = headerDetails;
    this.data = data;
  }

  public SheetContext(int sheetIndex, SheetHeader header, List<HeaderDetail> headerDetails, List<Object> data) {

    if (sheetIndex != header.getSheetIndex()) {
      throw new IllegalArgumentException("sheet index not equals header's sheet index");
    }

    this.sheetIndex = sheetIndex;
    this.header = header;
    this.headerDetails = headerDetails;
    this.data = data;
  }

  public SheetContext(int sheetIndex, String sheetName, SheetHeader header, List<HeaderDetail> headerDetails, List<Object> data) {

    if (sheetIndex != header.getSheetIndex()) {
      throw new IllegalArgumentException("sheet index not equals header's sheet index");
    }

    this.sheetIndex = sheetIndex;
    this.sheetName = sheetName;
    this.header = header;
    this.headerDetails = headerDetails;
    this.data = data;
  }

  /**
   * @return sheet index
   * @see Sheet#getIndex()
   */
  public int getSheetIndex() {
    return sheetIndex;
  }

  /**
   * @return sheet name
   * @see Sheet#getName()
   */
  public String getSheetName() {
    return sheetName;
  }

  /**
   * @return sheet header
   * @see SheetHeader
   */
  public SheetHeader getHeader() {
    return header;
  }

  /**
   * @return sheet header detail
   * @see HeaderDetail
   */
  public List<HeaderDetail> getHeaderDetails() {
    return headerDetails;
  }

  /**
   * @return the supplied object list to write on excel
   */
  public List<Object> getData() {
    return data;
  }
}
