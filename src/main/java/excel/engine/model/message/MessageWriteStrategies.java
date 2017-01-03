package excel.engine.model.message;

/**
 * message writer strategies
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class MessageWriteStrategies {

  private MessageWriteStrategies() {
    // default constructor
  }

  /**
   * use comment
   *
   * @see excel.engine.model.shapes.Comment
   */
  public static final String COMMENT = "comment";

  /**
   * use text box
   *
   * @see excel.engine.model.shapes.TextBox
   */
  public static final String TEXT_BOX = "text_box";
}
