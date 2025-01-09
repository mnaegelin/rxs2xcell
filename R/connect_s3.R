# aws.s3::bucket_exists(
#   bucket = "s3://xcell-public/",
#   region = "",
# )
#
# testimg <- aws.s3::get_object("test_img.jpg", bucket = "s3://xcell-public/", region = "")
#
# # save object in s3 to local path
# aws.s3::save_object("test_img.jpg", file = "test_img.jpg",
#                     bucket = "s3://xcell-public/", region = "")
#
# # put local file into S3
# aws.s3::put_object(file = "test.png", object = "/testA/test.png",
#                    bucket = "s3://xcell-public/", region = "")

