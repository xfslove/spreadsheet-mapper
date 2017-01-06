package spreadsheet.mapper.model.shapes;

import org.apache.poi.xssf.usermodel.XSSFClientAnchor;

import java.io.Serializable;

/**
 * the style of text box
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public class TextBoxStyle implements Serializable {

  public static final TextBoxStyle DEFAULT = new TextBoxStyle(0, 0, 3, 3, 251, 252, 155);

  private int col1;

  private int row1;

  private int col2;

  private int row2;

  private int red;

  private int green;

  private int blue;

  public TextBoxStyle(int col1, int row1, int col2, int row2, int red, int green, int blue) {
    this.col1 = col1;
    this.row1 = row1;
    this.col2 = col2;
    this.row2 = row2;
    this.red = red;
    this.green = green;
    this.blue = blue;
  }

  /**
   * @return 1-based
   * @see XSSFClientAnchor#getCol1()
   */
  public int getCol1() {
    return col1;
  }

  /**
   * @return 1-based
   * @see XSSFClientAnchor#getRow1()
   */
  public int getRow1() {
    return row1;
  }

  /**
   * @return 1-based
   * @see XSSFClientAnchor#getCol2()
   */
  public int getCol2() {
    return col2;
  }

  /**
   * @return 1-based
   * @see XSSFClientAnchor#getRow2()
   */
  public int getRow2() {
    return row2;
  }

  /**
   * the text box background color
   *
   * @return rgb color red
   */
  public int getRed() {
    return red;
  }

  /**
   * the text box background color
   *
   * @return rgb color green
   */
  public int getGreen() {
    return green;
  }

  /**
   * the text box background color
   *
   * @return rgb color blue
   */
  public int getBlue() {
    return blue;
  }
}
